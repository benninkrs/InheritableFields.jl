"""
    InheritableFields

Define abstract types with fields to be inherited by concrete subtypes.
"""
module InheritableFields
export @inheritable, @inheriting, validate_args
#export FieldDecl, map_symbols, type_decls, type_field_decls

# This seems to really work!!
# TODO: Tidy up.  Maybe move some stuff to TypeTools.
# TODO: Vet more thoroughly
# TODO: Handle Vararg parametric types
# TODO: Because type declarations and field declarations are stored as expressions,
#       namespaces are not utilized.  Thmis means same-named types in different
#       namespaces are not disambiguated.  Also, if an @inheriting type inherits an
#       @inheritable type definied in a different namespace, the inherited fields may
#       involve type names that are not known in the inheriting namespace.
#
#       Possible approaches:
#           - Qualify all names in type declarations by the module in which the
#               declaration was made, e.g. using @__MODULE__.  The challenge here is
#               knowing which parts of an expression need to be qualified. Perhaps
#               all symbols that are not typevars?
#           - Store declarations using evaluated types rather than expressions.
#               While this will work for type names alone, the challenge is what to
#               do with type parameters: They may need to be evaluated as well, but
#               may involve type parameters that aren't in the namespace.
#           - Create a dummy datatype with the fields.  Then translated field types can
#               can be obtained by looking them up with a fully parameterized dummy
#               datatype.  But we must make sure the type parameters are not constrained
#               since we might want to fill them with symbols or other type parameters.
#               Also, this will yield an actual type, not an expression ...
#
#          The first approach is in progress. So far, so good.
#          TODO: Move name qualification from @inheriting to process_typedef?
#                Maybe not, because then it would hhappen for @inheriting also ...
#          TODO: Qualify supertype name also.  But this makes it hard to use as a key.
#                Maybe use actual types for keys, but store parameter expressions
#                   separately.
#
# TODO: "Store" the type declarations in functions, which are global and can be precompiled.
#       This would also make it easier to look up types in namespaces, since we could
#       dispatch on the actual type instead of its name.  But then we should store
#       the formal parameter expressions separately from the types.
# TODO: Allow deriving from intermediary non-@inheritable types.  This will require extracting
#       the formal parameters from the type definition and mapping them, either directly or by
#       turning them into expressions first.


# Here's an example of the namespace issue.
# module MyModule
# using InheritableFields
# export A
#
# 	struct S{T}
# 		x::Vector{T}
# 	end
#
#
# 	@inheritable A{T} begin
# 			s::S{T}
# 		end
# end
#
# (in Main)
# using InheritableFields
# using Main.MyModule
# @inheriting struct B{X} <: A{X} end
#
# Since S is not exported to A, the field declaration s::S{T} from will not work in B.
# Thus s::S{T} needs to become s::MyModule.S{T}, which in B becomes s::MyModule.S{X}.
# Note that MyModule is necessarily a sufficient qualifier, since if MyModule were not
# not visible then we (probably) could not have referenced A; also, MyModule will encompass
# anything that was visible in the definition of A.


using MacroTools
using MacroTools: postwalk, prewalk
using TypeTools
using OrderedCollections

import TypeTools.map_symbols

# The declaration of each inheritable type are stored in a global registry.
# The declaration consists of:
#   - The type's name and formal parameters
#   - Its supertype's name and formal parameters
#   - The field names, their types, and optional default values
# All these are stored as expressions. When a concrete subtype is defined, these
# declarations are retrieved, translated into the formal parameters of the concrete type,
# and incorporated into the concrete type's definition.

# Types for Field declarations and type declarations
const Expression = Any
const FieldDecl = NamedTuple{(:name, :type, :default), Tuple{Symbol, Expression, Any}}
# (type_signature, supertype_signature, field_declarations)
const TypeDecl = Tuple{Expression, Expression, OrderedDict{Symbol, FieldDecl}}

# The global "registry" is a function that returns the type delcaration (::TypeDecl) of an
# @inheritable type (or returns nothing).
# TODO:  Currently accepts a symbol?  Change to accepting a type?
type_declaration(::Val{<:Any}) = nothing


# Argument validator for @inheritable types
function validate_args end

# This stores the signatures of the declared types and their supertypes
#const type_decls = Dict{Symbol, Tuple{Expression, Expression}}()
# This stores the expressions of fields declared for abtract types
#const type_field_decls = Dict{Symbol, OrderedDict{Symbol, FieldDecl}}()



"""
    @inheritable ‹type_decl› begin [fields] [validator] end

Define an abstract type with associated fields that are incorporated into concrete subtypes.

Each field declaration has the form `name[::Type] [ = defaultvalue]`.

The type declaration may also include a validator function to ensure that concrete
subtypes are constructed with valid field values. The validator must be named `validate_args`
and must have the same parameterization and signature that a default constructor would
have. The validator should either return valid arguments for a default constructor or
throw an exception.

# Example
```
@inheritable A{T<:Number}
begin
    x::T = zero(T)
    y
    function validate_args(x, y)
        x < zero(T) || error("x must be non-negative")
        return (x,y)
    end
end
```
See also:  [`@inherit`](@ref)
"""
macro inheritable(type_decl, body = :(begin end) )
    (type_sig, type_name, type_params, type_qualparams, unbound_sig, super_sig, field_decls, validators) = process_typedef(type_decl, body)

    caller_mod = __module__
    #@info "Defining inheritable type $type_sig <: $super_sig in module $caller_mod"

    # create a default "validator" if none was provided
    if isempty(validators)
        field_names = keys(field_decls)
        push!(validators, :( InheritableFields.validate_args(::Type{$unbound_sig}, $(field_names...)) where {$(type_qualparams...)} = ($(field_names...),) ) )
    end

    caller_name = nameof(caller_mod)
    for i in eachindex(field_decls)
        fdecl = field_decls[i]
        #print("Qualifying $(fdecl.type) as ")
        new_type = qualify_symbols(fdecl.type, type_params, caller_name)
        new_default = qualify_symbols(fdecl.default, type_params, caller_name)
        fdecl = (name = fdecl.name, type = new_type, default = new_default)
        #println(fdecl.type)
        field_decls[i] = fdecl
    end
    newtype_decl = TypeDecl((unbound_sig, super_sig, field_decls))

    expr = quote
#                try
                   Base.@__doc__ abstract type $type_sig <: $super_sig end
                   $(validators...)
                   InheritableFields.type_declaration(::Val{$(QuoteNode(type_name))}) = $newtype_decl
#            catch e
#                delete!(InheritableFields.type_declarations, $(QuoteNode(type_name)));
#                rethrow(e)
#            end
        end
#    println( MacroTools.prewalk(rmlines, esc(expr)))
    return esc(expr)
#    return MacroTools.prewalk(rmlines, esc(expr))
end



"""
    @inheriting [mutable] struct ‹type_decl› [fields] [validator] end

Define a concrete type that inherits fields from supertypes declared with `@inheritable`.
The fields of all `inheritable` supertypes are automatically incorporated into the
definition of the concrete type, in order of increasingly specialized supertypes.
# EXample
```
@inheritable A{T}
begin
    x::T = zero(T)
    y
end

@inheriting struct B{S} <: A{S}
    z::String = "hello"
end
```
Then `B` has fields `x::S, y::Any, z::String`. (Note that the type parameters of `B` are
propagated to `A`.)  An instane of `B` can be created by the default constructor,
`B{Float64}(1.2, -5, "bye")` are one of several convenience constructors that are
automatically created:
```
# Construct with keyword arguments
B(; x::S = zero(S), y, z::String = "hello") where {S} = B{S}(x, y, z)
B{S}(; x::S = zero(S), y::Any, z::String = "hello") where {S} = B{S}(x, y, z)

# Construct from another instance, modifying selected fields with keyword arguments
B(obj::B; x::S = obj.x, y::Any = obj.y, z::String = obj.z) where {S} = B{S}(x, y, z)
B{S}(obj::B{S}; x::S = obj.x, y::Any = obj.y, z::String = obj.z) where {S} = B{S}(x, y, z)

```
(The second member of each pair is not created if B has no type parameters.)

See also: [`@inheritable`](@ref)
"""
macro inheriting(typedef)
    return esc(inheriting(typedef))
end

# Function version. More convenient for calling from other macros
function inheriting(typedef)
    # parse the signature
    typedef.head == :struct || error("@inherit must be followed by a structure definition")
    ismutable = typedef.args[1]

    type_decl = typedef.args[2]
    body = typedef.args[3]
    inheriting(type_decl, body, ismutable = ismutable)
end


function inheriting(type_decl, body; ismutable)
    (type_sig, type_name, type_params, type_qualparams, unbound_sig, super_sig, field_decls, validators) = process_typedef(type_decl, body)

    # @info "Defining inherited type $type_sig <: $super_sig"

    newtype_decl =  TypeDecl((unbound_sig, super_sig, field_decls))

    # println("Adding declaration: ", type_declarations[type_name][1], " <: ", type_declarations[type_name][2])


    # Assemble the field declarations for this type and all ancestor types.
    # The search will proceed up the type hierarchy, but we will eventually reverse the
    # order so that fields are listed in the order defined from ancestor down to concerete type.

    struct_field_decls = Expression[]
    kw_args1 = Expression[]
    kw_args2 = Expression[]
    constr_args = Expression[]

    ancestor_name = type_name
    ancestor_params = type_params
    ancestor_unbound_sig = unbound_sig
    formal_sig = type_sig
    next_sig = super_sig
    formal_params::Vector{Symbol} = type_params


    while ancestor_name != :Any
        # @info "  Retrieving fields of $ancestor_name{$(ancestor_params...)}"

        if ancestor_name == type_name || type_declaration(Val(ancestor_name)) != nothing
            # (formal_sig, next_sig) = type_decls[ancestor_name]
            # field_decls = type_field_decls[ancestor_name]
            if ancestor_name != type_name
                (formal_sig, next_sig, field_decls) = type_declaration(Val(ancestor_name))
                (_, formal_params, _) = parse_typesig(formal_sig)
            end
            # println("processing ", ancestor_name)
            # println("  ancestor_params = ", ancestor_params)
            # println("  ancestor_unbound_sig = ", ancestor_unbound_sig)
            # println("  formal_sig = ", formal_sig)
            # println("  formal_params = ", formal_params)
            # println("  next_sig = ", next_sig)
            _struct_field_decls = Expression[]
            _kw_args1 = Expression[]
            _kw_args2 = Expression[]
            if length(ancestor_params) != length(formal_params)
                @info "type = $ancestor_name"
                @info "ancestor_params = $ancestor_params"
                @info "formal_params = $formal_params"
                error("The declaration of $ancestor_name or one of its subtypes did not specify all required parameters")
            end
            # println("Processing type $ancestor_name with parameters {$(ancestor_params...)}, formal parameters {$(formal_params...)}")
            for fdecl in values(field_decls)
                # println("  Adding field ", fdecl.name)
                # translate the field declaration to the ancestor's parameters
                # print("inheriting field $fdecl as ")
                fdecl = map_symbols(fdecl, formal_params, ancestor_params)
                # @info "   Inheriting field $fdecl"
                # add the declaration w/o the default value to the struct's declared fields
                push!(_struct_field_decls, plain_field_decl(fdecl))
                # add the full declaration to the list of arguments for the keyword validator
                push!(_kw_args1, kw_field_decl(fdecl))
                push!(_kw_args2, kw_field_decl_obj(fdecl))
            end
            prepend!(struct_field_decls, _struct_field_decls)
            prepend!(kw_args1, _kw_args1)
            prepend!(kw_args2, _kw_args2)
            pushfirst!(constr_args, :( InheritableFields.validate_args($ancestor_unbound_sig, $(keys(field_decls)...) )... ) )
            # When we handle non-@inheritable ancestors, this should go outside the if-else-end
            next_sig = map_symbols(next_sig, formal_params, ancestor_params)
            (ancestor_name, ancestor_params, _) = parse_typesig(next_sig)
            if isempty(ancestor_params)
                ancestor_unbound_sig = ancestor_name
            else
                ancestor_unbound_sig = :( $(ancestor_name){$(ancestor_params...)} )
            end
        else
            @warn "In defining $type_name, $ancestor_name is not an `@inheritable` type. No more ancestors will be `@inherit`ed."
            break
        end
    end

    # Define the keyword constructors
    kwconstrs = Expr[]

    # keywords with inferred parameterization
    push!(kwconstrs, :( $type_name(; $(kw_args1...)) where {$(type_qualparams...)} = $unbound_sig($(constr_args...)) ) )

    # Copy-with-modifications constructor
    push!(kwconstrs, :( $type_name(obj::$type_name; $(kw_args2...)) where {$(type_qualparams...)} = $unbound_sig($(constr_args...)) ))

    # Same thing, but with explicit type parameters
    if ~isempty(type_params)
        push!(kwconstrs, :( $unbound_sig(; $(kw_args1...)) where {$(type_qualparams...)} = $unbound_sig($(constr_args...)) ) )
        push!(kwconstrs, :( $unbound_sig(obj::$unbound_sig; $(kw_args2...)) where {$(type_qualparams...)} = $unbound_sig($(constr_args...)) ))
    end
    struct_def = Expr(:struct, ismutable, :( $type_sig <: $super_sig ), Expr(:block, struct_field_decls...))

    expr =  quote
                Base.@__doc__ $struct_def
                $(validators...)
                $(kwconstrs...)
                copy(obj::$type_name) = $type_name(obj)
                InheritableFields.type_declaration(::Val{$(QuoteNode(type_name))}) = $newtype_decl
            end
    # println(expr)
    return expr
    # return MacroTools.prewalk(rmlines, esc(expr))
end




# Process the components of a type definition, connsisting of a "declaration" and a "body".
# The "declaration" is the name of the type, its parameterization, and its supertype,
# e.g.  A{T<:Number} <: B{T}.
#   A{T<:Number} is the "signature"
#   A{T} is the "unbound signature" -- needed for method arguments
#   {T <: Number} is the parameterization -- needed for "where" clauses
#   T is the "type var".
# The "body" are all the lines after the type declaration.
#   In the body we allow (1) field declarations with optional types and optional default
#   values, and (2) functions that have the same signature as the default constructor.
function process_typedef(type_decl::Expression, body::Expression)
    # Parse the declaration
    (type_sig, super_sig) = parse_typedecl(type_decl)
    (type_name, type_params, type_qualparams) = parse_typesig(type_sig)

    # Construct a version of the type signature without only unbound parameters (for
    # function signatures)
    if isempty(type_params)
        unbound_sig = type_sig
    else
        unbound_sig = :( $(type_name){$(type_params...)} )
    end

    # Parse the body
    field_decls = OrderedDict{Symbol, FieldDecl}()
    validators = Expression[]

    # Evidently @capture ignores LineNumberNodes, which is why it is useful
    @capture(body, begin body_decls__ end) || error("Body must be a block.")
    # loop over lines of the body
    for decl in body_decls
        # println("processing body declaration ", decl)
        isfunction = @capture(decl, ((func_(args__) | func_(args__) where params_) = body_ ) | ( function (func_(args__) | func_(args__) where params_) body_ end ))

        if isfunction
            # Check whether we have a well-formed validator
            (func == :validate_args) || error("The only function that may be defined in an @inheritable type must be named 'validate_args'.")
            (params == nothing) || @warn "Ignoring the `where {...}` clause of the validator; the necessary `where` cluase is automatically derived from the type declaration."

            # We appear to have a validator.  Make sure it's the only one
            length(validators) > 0 && error("Type declaration can have at most one validator")

            # Modify the validator:  Qualify the name and add the type as the first argument
            decl = postwalk(decl) do ex
                    if (ex isa Expr) && (ex.head == :call) && (ex.args[1] == :validate_args)
                        ex.args = [:(InheritableFields.validate_args); :(::Type{$unbound_sig}); ex.args[2:end]]
                    end
                    return ex
                end

            # Add a "where" clause with the type's parameters
             decl.args[1] = Expr(:where, decl.args[1], type_params...)
            push!(validators, decl)
        elseif @capture(decl, ((field_name_::field_type_  | field_name_) = def_val_) | (field_name_::field_type_ | field_name_))
            (field_type === nothing) && (field_type = :Any)
            field_decls[field_name] = FieldDecl((field_name, field_type, def_val))
        else
            error("In definition of type $type_name, could not parse declaration $decl")
        end
    end

    # create a default "validator" if none was provided
    if isempty(validators)
        field_names = keys(field_decls)
        push!(validators, :( InheritableFields.validate_args(::Type{$unbound_sig}, $(field_names...)) where {$(type_qualparams...)} = ($(field_names...),) ) )
    end

    return (type_sig, type_name, type_params, type_qualparams, unbound_sig, super_sig, field_decls, validators)
end


map_symbols(fd::FieldDecl, from_syms, to_syms) = FieldDecl(map(ex->map_symbols(ex, from_syms, to_syms), fd))


# Return the expression for a keyword-type of field declaration
function kw_field_decl(fdecl::FieldDecl)
    ex = Expr(:(::), fdecl.name, fdecl.type)
    if fdecl.default != nothing
        ex = Expr(:kw, ex, fdecl.default)
    end
    return ex
end

function kw_field_decl_obj(fdecl::FieldDecl)
    ex = Expr(:(::), fdecl.name, fdecl.type)
    ex = Expr(:kw, ex, :( obj.$(fdecl.name) ))
    return ex
end

# Return the expression for a field declaration, omitting the default value
plain_field_decl(fdecl::FieldDecl) = Expr(:(::), fdecl.name, fdecl.type)



# Qualify symbols in an expression with namespace
function qualify_symbols(expr, exclude_syms, namespace)
    qexpr = postwalk(expr) do ex
            if (ex isa Symbol) && !(ex in exclude_syms)
                ex =  :( $namespace.$ex )
            end
            return ex
        end
end

end

# -----------------------------------------------------------
#   OBSOLETE STUFF
#

# Name of the associated data structure
# dataname = Symbol("$(typename)Data")
# if isempty(typeparams)
#     datasig = :( $(dataname) )
# else
#     datasig = :( $(dataname){$(typeparams...)} )
# end


#     # Functions that store fields' default values
#     default_funs = Expression[]
#     for (field_name, val) in defaults
#         # We need defaultvalue_expr to return an expression with input-dependent parameters.
#         #println("val = $val")
#         for typevar in typevars
#             val = MacroTools.postwalk(item -> (item == typevar) ? Expr(:$, typevar) : item, val)
#         end
#         val = Expr(:quote, val)
#         #println("val = $val")
#         push!(default_funs, MacroTools.prewalk(rmlines, quote
# #            InheritableFields.hasdefault(::Type{$unboundsig}, ::Val{$(QuoteNode(field_name))}) where {$(typeparams...)} = true
#             InheritableFields.defaultvalue_expr(::Val{$(QuoteNode(field_name))}, ::Val{$(QuoteNode(typename))}, $(typevars...)) = $val
#         end
#         ))
#         #show(default_funs[end])
#     end
#
#     # Create a function that maps a typename and formal parameters to it's supertype's name
#     # and formal parameters
#     (supername, superparams) = parse_typeexpr(supersig)
#     superparam_exprs = superparams
#     println("superparams_exprs = ", superparam_exprs)
#     # dump(superparams_expr)
#     for i in eachindex(superparam_exprs)
#         for typevar in typevars
#             if superparam_exprs[i] == typevar
#                 superparam_exprs[i] = Expr(:quote, Expr(:$, typevar))
#             end
#         end
#     end
#     supertype_fun = :( InheritableFields.supertype_expr(::Val{$(QuoteNode(typename))}, ($(typevars...),)) = ($(QuoteNode(supername)), ($(superparam_exprs...),) ) )
#


# Create the code to be evaluated
# - Declare the abstract type
# - Define the validators
# - Record the new type in the table of inheritable types
# - Record its field declarations
# We do these last two steps in the evaluated expression rather than within this macro,
# in case something fails. and their supertypes
# -
# quot_type_name = QuoteNode(type_name)
# quot_unbound_sig = QuoteNode(unbound_sig)
# quot_super_sig = QuoteNode(super_sig)
