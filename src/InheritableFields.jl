"""
    InheritableFields

Define abstract types with fields to be inherited by concrete subtypes.
"""
module InheritableFields
export @abstract, @immutable, @mutable
#export FieldDecl, map_symbols, type_decls, type_field_decls


#=
Overview:

@abstract defines an abstract type with fields to be inherited by concrete subtypes.
@(im)mutable defines a (im)mutable struct type that includes the fields of all its @abstract ancestors.

If we want a hierarchy A :> B :> C and and want concrete instances of A,B,C, we have
to use @(im)mutable to define types ConcreteA <: A, ConcreteB <: B, and ConcreteC <: C.
This is unavoidable in Julia since concrete types (i.e. types with fields) cannot be subtyped.
=#

#=
How it is implemented:

When an @abstract type is declared:
- A corresponding abstract type is defined
- Validation methods appearing in the body are defined
- The details of the type declaration are stored in a global way.
	These details include the current module, the signature, and the field declarations.

When a @mutable or @immutable is declared:
- A corresponding struct is defined, with all the inherited fields and validating inner constructors
- Validation methods appearing in the body are defined
- The details of the type declaration are stored in a global way.
	These details include the current module, the signature, and the field declarations.

	This is done by defining a method of the type_declaration function that returns those details. The reason we do it this way instead of using a Dict, is ... ? 

=#


# TODO: Vet more thoroughly
# TODO: Handle Vararg parametric types
# TODO: Tidy up.  Maybe move some stuff to TypeTools.
# TODO: Allow deriving from intermediary non-@abstract types.  This will require extracting
#       the formal parameters from the type definition and mapping them, either directly or by
#       turning them into expressions first.
# TODO: For storing type declarations, consider returning to a global Dict instead
#		Original reasoning for using functions:
#			Functions are global and can be precompiled.
#			This would also make it easier to look up types in namespaces, since we could
#			dispatch on the actual type instead of its name.  But then we should store
#			the formal parameter expressions separately from the types.
#		I think  those arguments are fallacious.  



using MacroTools
using MacroTools: postwalk, prewalk
using TypeTools
import TypeTools.map_symbols
using OrderedCollections


# The declaration of each type are stored in a global registry.
# The declaration consists of:
#	- The module in which the type is defined
#	- The type's name and formal parameters
#	- Its supertype's name and formal parameters
#	- The field names, their types, and optional default values
# All these are stored as expressions.
# When a concrete subtype is defined, these # declarations are retrieved, translated
# into the formal parameters of the concrete type, and incorporated into the concrete
# type's definition.

# Type aliases for data structures that store field declarations and type declarations
const Expression = Any
const FieldDecl = NamedTuple{(:name, :type, :default), Tuple{Symbol, Expression, Any}}
const TypeDecl = Tuple{Module, Expression, Expression, OrderedDict{Symbol, FieldDecl}}

# Global storage of type declarations
# const type_decls = Dict{Type, TypeDecl}}()
# Instead, we'll use a function 
type_declaration(::Type) = nothing

# Function to validate arguments for constructors of types declared using this package.
# Fallback is to simply return the arguments.
validate(::Type, args...) = args


"""
    @abstract ‹type_decl› begin [fields] [validator] end

Define an abstract type with fields to be incorporated into concrete subtypes.

Each field declaration has the form `name[::Type] [ = defaultvalue]`.

The type declaration may also include a validator function to ensure that concrete
subtypes are constructed with valid field values. The validator must be named `validate`
and must have the same parameterization and signature that a default constructor would
have. The validator should either return valid arguments for a default constructor or
throw an exception.

# Example
```
@abstract A{T<:Number} begin
	s::String
	x::T = zero(T)
	function validate(s, x)
		x >= zero(T) || error("x must be non-negative")
   	return (s, x)
   end
end
```
See also: [`@mutable`](@ref), [`@immutable`](@ref)
"""
macro abstract(type_decl, body = :(begin end) )
    (type_sig, type_name, type_params, type_qualparams, unbound_sig, super_sig, field_decls, validators) = process_typedef(type_decl, body)

    newtype_decl = TypeDecl((__module__, unbound_sig, super_sig, field_decls))

    expr = quote
					Base.@__doc__ abstract type $type_sig <: $super_sig end
					$(validators...)
					InheritableFields.type_declaration(::Type{$type_name}) = $newtype_decl
			end
    return esc(expr)
end



"""
    @mutable ‹type_decl› begin [fields] [validator] end
    @immutable ‹type_decl› begin [fields] [validator] end

Define a concrete struct type that inherits fields from supertypes declared with `@abstract`.
The fields of all @abstract supertypes are automatically incorporated into the
definition of the concrete type, in order of increasingly specialized supertypes.
# Example
```
@abstract A{T<:Number} begin
	s::String
	x::T = zero(T)
	function validate(s, x)
		x >= zero(T) || error("x must be non-negative")
   	return (s, x)
   end
end

@immutable B{S} <: A{S} begin
    i::Int
	 y
end
```
Then `B` has fields `s::String, x::S, i::Int, z::Any`. (Note that the type parameters of `B` are
propagated to `A`.)  An instance of `B` can be created by the default constructor, 
`B{Float64}("hello", 1.2, -5, true), or one of several convenience constructors that are
automatically created:
```
# Construct with keyword arguments
B(; s::String, x::S = zero(S), i::Int, y::Any) where {S} = B{S}(s, x, i, y)
B{S}(; s::String, x::S = zero(S), i::Int, y::Any) where {S} = B{S}(s, x, i, y)

# Construct from another instance, modifying selected fields with keyword arguments
B(obj::B; s::String = obj.s, x::S = obj.x, i::Int = obj.i, y::Any = obj.y) where {S} = B{S}(s, x, i, i)
B{S}(obj::B{S}; x::S = obj.x, y::Any = obj.y, z::String = obj.z) where {S} = B{S}(s, x, i, y)

```
(The second member of each pair is not created if B has no type parameters.)

See also: [`@abstract`](@ref)
"""
macro immutable(type_decl, body = :(begin end) )
	caller = __module__
    return esc(define_concrete_type(caller, type_decl, body; ismutable = false))
end


macro mutable(type_decl, body = :(begin end) )
	caller = __module__
	return esc(define_concrete_type(caller, type_decl, body; ismutable = true))
end


# Function version. More convenient for calling from other macros
function define_concrete_type(caller, type_decl, body; ismutable)
	(type_sig, type_name, type_params, type_qualparams, unbound_sig, super_sig, field_decls, validators) = process_typedef(type_decl, body)
	if isempty(type_params)
		new_sig = :( new )
	else
		new_sig = :( new{$(type_params...)} )
	end

	newtype_decl =  TypeDecl((caller, unbound_sig, super_sig, field_decls))

	# Assemble the field declarations for this type and all ancestor types.
	# The search will proceed up the type hierarchy, but we will eventually reverse the
	# order so that fields are listed in the order defined from ancestor down to concerete type.

	struct_field_decls = Expression[]
	valid_args = Expression[]		# argument list for default constructor
	kw_args = Expression[]			# arguments list for  keyword constructor
	obj_kw_args = Expression[]		# arguemtns list for object-base keyword constructor

	# ancestor will loop from the type being defined up through the chain of ancestors
	ancestor_name = type_name
	ancestor_params = type_params
	ancestor_unbound_sig = unbound_sig
	formal_sig = type_sig
	next_sig = super_sig
	formal_params::Vector{Symbol} = type_params
	context = caller
	# println("calling module is $caller")

	while ancestor_name != :Any
      # @info "  Retrieving fields of $ancestor_name{$(ancestor_params...)}"

		_struct_field_decls = Expression[]
		_kw_args = Expression[]
		_obj_kw_args = Expression[]

		if ancestor_name != type_name
			# This is a true ancestor.  Retrieve its fields.
			
			# Obtain the ancestor type from the name
			ancestor_type = Base.eval(context, ancestor_name)
			# ancestor_type = context.eval(ancestor_name)

			type_decl = type_declaration(ancestor_type)
			if isnothing(type_decl)
				@warn "In defining $type_name, $ancestor_name is not an `@abstract` type. No more ancestors will be inherited."
				break
			end

			# Retrieve the field declarations
			# (formal_sig, next_sig) = type_decls[ancestor_name]
			# field_decls = type_field_decls[ancestor_name]
			(context, formal_sig, next_sig, field_decls) = type_decl
			(_, formal_params, _) = parse_typesig(formal_sig)

			# println("  ancestor_params = ", ancestor_params)
			# println("  ancestor_unbound_sig = ", ancestor_unbound_sig)
			# println("  formal_sig = ", formal_sig)
			# println("  formal_params = ", formal_params)
			# println("  next_sig = ", next_sig)

			if length(ancestor_params) != length(formal_params)
					@info "ancestor_params = $ancestor_params"
					@info "formal_params = $formal_params"
					error("The declaration of $ancestor_name did not specify all required parameters")
			end
		end

		# Parse the field declarations
		# println("Processing type $ancestor_name with parameters {$(ancestor_params...)}, formal parameters {$(formal_params...)}")
		for fdecl in values(field_decls)
			# translate the field declaration to the ancestor's parameters

			# Map the formal parameters from subtype to supertype
			fdecl = map_symbols(fdecl, formal_params, ancestor_params)

			# Qualify all symbols (excluging type vars) by the Module where they were used
			new_type = qualify_symbols(fdecl.type, type_params, context)
			new_default = qualify_symbols(fdecl.default, type_params, context)
			fdecl = FieldDecl((fdecl.name, new_type, new_default))

			# add the declaration w/o the default value to the struct's declared fields
			push!(_struct_field_decls, plain_field_decl(fdecl))

			# add the declaration w/ default value /to the list of arguments for the keyword validator
			push!(_kw_args, kw_field_decl(fdecl))
			push!(_obj_kw_args, kw_field_decl_obj(fdecl))
		end

		# Prepend the current type's field declarations to list of all inherited field declarations  
		prepend!(struct_field_decls, _struct_field_decls)
		prepend!(kw_args, _kw_args)
		prepend!(obj_kw_args, _obj_kw_args)

		# Prepend the validator call to the list of validator calls in the constructor
		pushfirst!(valid_args, :( InheritableFields.validate($ancestor_unbound_sig, $(keys(field_decls)...) )... ) )

		next_sig = map_symbols(next_sig, formal_params, ancestor_params)
		(ancestor_name, ancestor_params, _) = parse_typesig(next_sig)
		if isempty(ancestor_params)
				ancestor_unbound_sig = ancestor_name
		else
				ancestor_unbound_sig = :( $(ancestor_name){$(ancestor_params...)} )
		end
	end	# loop over type hierarchy


	# Create the constructor methods
	constrs = Expr[]
	kwconstrs = Expr[]

	# validating default constructor
	push!(constrs, :( $type_name($(struct_field_decls...)) where {$(type_qualparams...)} = $new_sig($(valid_args...)) ) )

	# keyword constructor
	push!(kwconstrs, :( $type_name(; $(kw_args...)) where {$(type_qualparams...)} = $unbound_sig($(struct_field_decls...)) ) )

	# Copy-with-modifications constructor
	push!(kwconstrs, :( $type_name(obj::$type_name; $(obj_kw_args...)) where {$(type_qualparams...)} = $unbound_sig($(struct_field_decls...)) ))

	# Same thing, but with explicit type parameters
	if ~isempty(type_params)
		push!(constrs, :( $unbound_sig($(struct_field_decls...)) where {$(type_qualparams...)} = $new_sig($(valid_args...)) ) )
		push!(kwconstrs, :( $unbound_sig(; $(kw_args...)) where {$(type_qualparams...)} = $unbound_sig($(struct_field_decls...)) ) )
		push!(kwconstrs, :( $unbound_sig(obj::$unbound_sig; $(obj_kw_args...)) where {$(type_qualparams...)} = $unbound_sig($(struct_field_decls...)) ))
	end

	# Create the struct definition
	struct_def = Expr(:struct, ismutable, :( $type_sig <: $super_sig ), Expr(:block, struct_field_decls..., constrs...))

	# Assemble everything
	expr =  quote
					Base.@__doc__ $struct_def
					$(validators...)
					$(kwconstrs...)
					copy(obj::$type_name) = $type_name(obj)
					InheritableFields.type_declaration(::Type{$type_name}) = $newtype_decl
			end
	return rmlines(expr)
end



# Process the components of a type definition, connsisting of a "declaration" and a "body".
# The "declaration" is the name of the type, its parameterization, and its supertype,
# e.g.  A{T<:Number} <: B{T} begin ... end.
# The type declaration is the A{T<:Number} <: B{T}.
# The "body" is the begin ... end block.
# In the body we allow (1) field declarations with optional types and optional default
# values, and (2) functions that have the same signature as the default constructor.
# Outputs:
#	type signature:	A{T<:Number}
# 	type name:			A
#	type_params:		[T]
#	type_qualparams	[T<:Number]
#	unbound_sig			A{T}
#	super_sig			B{T}
#	field_decls
#	validator
# All the above are expressions.
# The "body" are all the lines after the type declaration.
#(type_sig, type_name, type_params, type_qualparams, unbound_sig, super_sig, field_decls, validators)
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
			(func == :validate) || error("The only function that may be defined in an @abstract type must be named 'validate'.")
			(params == nothing) || @warn "Ignoring the `where {...}` clause of the validator; the necessary `where` cluase is automatically derived from the type declaration."

			# We appear to have a validator.  Make sure it's the only one
			length(validators) > 0 && error("Type declaration can have at most one validator")

			# Modify the validator:  Qualify the name and add the type as the first argument
			decl = postwalk(decl) do ex
						if (ex isa Expr) && (ex.head == :call) && (ex.args[1] == :validate)
							ex.args = [:(InheritableFields.validate); :(::Type{$unbound_sig}); ex.args[2:end]]
						end
						return ex
					end

			# Add a "where" clause with the type's parameters
				decl.args[1] = Expr(:where, decl.args[1], type_params...)
			push!(validators, decl)
		elseif @capture(decl, ((field_name_::field_type_  | field_name_) = def_val_) | (field_name_::field_type_ | field_name_))
			if isnothing(field_type)
				isnothing(def_val) ? field_type = :Any : field_type = :( typeof($def_val) )
			end
			field_decls[field_name] = FieldDecl((field_name, field_type, def_val))
		else
			error("In definition of type $type_name, could not parse declaration $decl")
		end
	end

	# create a default "validator" if none was provided
	# if isempty(validators)
	# 	field_names = keys(field_decls)
	# 	# Why is this validatet instead of validate?  What is the prupose of this?  Seems to be wrong
	# 	push!(validators, :( InheritableFields.validatet =(::Type{$unbound_sig}, $(field_names...)) where {$(type_qualparams...)} = ($(field_names...),) ) )
	# end

	# @info "type_sig = $type_sig"
	# @info "type_name = $type_name"
	# @info "type_params = $type_params"
	# @info "type_qualparams = $type_qualparams"
	# @info "unbound_sig = $unbound_sig"
	# @info "super_sig = $super_sig"
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