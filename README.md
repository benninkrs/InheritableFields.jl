# InheritableFields.jl

[InheritableFields.jl](https://github.com/benninkrs/InheritableFields.jl) provides a convenient, robust way to define abstract types with fields that are ultimately inherited by concrete subtypes.
(Similar functionality is offered by many other packages, including [Classes.jl](https://github.com/rjplevin/Classes.jl), [ConcreteAbstractions.jl](https://github.com/tbreloff/ConcreteAbstractions.jl),
[OOPMacro.jl](https://github.com/ipod825/OOPMacro.jl), [ObjectOriented.jl](https://github.com/Suzhou-Tongyuan/ObjectOriented.jl), and [Inherit.jl](https://github.com/mind6/Inherit.jl); 
see also [Mixers.jl](https://github.com/rafaqz/Mixers.jl) and [ReusePatterns.jl](https://github.com/gcalderone/ReusePatterns.jl).)

This package does not aim to broadly recreate an object-oriented programming style in Julia, nor does it aim to provide a way to define and/or enforce interfaces.

## Basic Usage

Use the macro `@abstract` to define an abstract type with associated fields:
```
@abstract A{T<:Number} begin
    s::String
    x::T 
end
```
If the type has no fields, the `begin end` block may be omitted from the definition. Abstract types can be subtyped:
```
@abstract B{T} <: A{T} begin
    i::Int
end
```
Use `@mutable` or `@immutable` to create mutable or immutable concrete type (struct) that inherits the fields of all its `@abstract` supertypes and can introduce additional fields:
```
@immutable C{T} <: B{T} begin
    b::Bool
end
```
In this example, instances of type `C{T}` will have fields `s::String`, `x::T`, `i::Int`, and `b::bool`, in that order. An instance of a `@mutable` or `@immutable` type can be constructed in the usual way, by calling the type with a value for each field in order.  However, construction using keywords (described below) is more flexible and powerful. 

Note that only abstract types can be inherited, in accordance with Julia's fundamental design.  To achieve what ammounts to a hierarchy of _concrete types_, say `Person >: Employee :> Salaried`, one can use the following approach:  First. create a hierarchy of `@abstract` types, e.g. `AbstractPerson :> AbstractEmployee >: AbstractSalaried`.  Second, define methods that dispatch on these abstract types.  Finally, define a concrete type for each abstract type in the hierarachy (e.g. `@mutable Person <: AbstractPerson`, `@mutable Employee <: AbstractEmployee`, etc.).

<!-- c = C("hello", 1.2, -6, true)
```
`c` is of type `C{Float64}` with fields `s = "hello"`, `x = 1.2`, `i = -6`, `b = true` in that order.  The general principle is that the fields of the concrete type and all `@abstract` supertypes are concatenated in order from the most abstract supertype to the least abstract.  For this reason, the fieldnames of a subtype must be distinct from those of its aancestor types. -->


## Advanced Usage

Construction of an object with fields defined in separate places can be tricky.
To facilitate this, the macros `@mutable` and `@immutable` automatically define several constructors:
* An inner constructor that resembles a default constructor, but allows each type in the hierarchy to validate its input arguments.
* A keyword-based outer constructor in which values may be assigned to named fields in any order, and omitted fields take default values provided in the type definitions.
* A keyword-based outer constructor that uses an existing object to provide default values.
If the object has type parameters, two versions of each constructor are created: one with explicit type parameters, and one in which the type parameters are inferred from the arguments.

A `copy` method for the type is also created.

### Argument Validation

Within the body of an `@abstract`, `@mutable`, or `@immutable` type definition, one can implement a special method to validate construction values for the fields introduced by that type. For example, suppose type `A` requires `x` to be nonnegative.  This can be enforced as follows:
```
@abstract A{T<:Number} begin
    s::String
    x::T
    function validate(s, x)
        x >= zero(T) || error("x must be non-negative")
        return (s, x)
    end
end
```
Suppose types `B` and `C` are defined as above. Attempting to construct an instance of `C` (a subtype of `A`) with a negative value for `x` will produce an error:
```
c = C(; i = -6, b = true, x = -1.2, s = "hello")
ERROR: x must be non-negative
```
A `validate` method defined in an `@abstract`, `@mutable`, or `@immutable` definition is called whenever an instance based on that type is constructed. It is passed candidate values for the type's fields as if it were the default inner constructor. The method should either return a tuple of field values or throw an exception.

If no validation method is provided, a fallback method that simply returns the input arguments is used.

### Keyword Constructors

In addition to the standard constructor, a keyword-based constructor is defined for each `@mutable` or `@immutable` type.  This allows field values to be specified in any order using the field names as keywords:
```
c = C(; i = -6, x = 1.2, b = true, s = "hello")  # == C{Float64}("hello", 1.2, -6, true)
```
Besides providing increased readibility and robustness to field order, the keyword constructor allows the use of default values (explained next). 


### Default Values

Any field declaration may optionally include a default value by expressing it as an assignment:
```
@abstract A{T<:Number} begin
    s::String = "goodbye"
    x::T
end
```
If a default value is provided, the field's type may be omitted (but see the Limitations below); in this case the field's type is taken to be that of the provided value.
The default value of a field is used when the keyword constructor is invoked and no value is provided for that field:
```
c = C(; i = -6; b = true; x = 1.2)  # == C{Float64}("goodbye", 1.2, -6, true)
```
Default values can also be obtained from an existing instance:
```
new_c = C(c; s = "goodbye")
```
In this case, values for the unspecified fields are copied from the provided instance.



## Hygiene

When a `@mutable` or `@immutable` type is defined, formal and literal type parameters are propagated up the chain of type definitions so that inherited fields are expressed in terms of the correct type parameters.

Similarly, all symbols appearing in a type definition are implicitly qualified by the module in which the definition occurs, so that they will be resolved correctly even when subtypes are defined in different modules.

<!--
For example, if `C` were defined as
```
@immutable C{U,S} <: B{S}
begin
	b::U
end
```
then in `C("hi", Complex(0.1, -2.3), 5, true)`, the field `x::T` from `A{T}` would be `x::Complex{Float64}` because the `S` in `C{U,S}` is inferred to be Complex{Float64}, which is then mapped to `B{S}`, which is mapped to `A{S}`, which is mapped to `x::S`.

Similarly, type definitions will be evaluated correctly even if defined in different modules.  This is because all non-parameter symbols appearing in field declarations are implicitly qualified by the module in which they are originally defined.
-->


## Limitations

Within a type definition, if the default value given for a field depends on a type parameter, for example `x = zero(T)`, the field's type cannot be automatically inferred.  In this case the type must be explicitly provided, e.g. `x::T = zero(T)`. 

`Vararg` type parameters have not been tested may not work correctly.

<!--
`validate` methods should only be defined within the bodies of `@abstract`, `@mutable`, or `@immutable` type definitions.
-->

## Implementation Details

The  `@abstract` macro:
1. Defines the specified type as `abstract type`.
2. Defines the `validate` method for the type (if provided).
3. Stores the definition of the type's fields for later retrieval.

 The `@mutable` or `@immutable` macro:
1. Defines the specified type as `struct` or `mutable struct`.
    - Looks up the inherited fields of all `@abstract` supertypes and includes them.
    - Creates an inner constructor that calls the `validate` method for each (super)type the corresponding arguments.
2. Defines the `validate` method for the type (if provided).
3. Defines keyword-based outer constructors.
4. Defines a copy method for the type.

`validate` methods are actually defined slightly differently than how the appear in the type definition: They have an additional argument at the front, namely, the type for which the method is defined.  This allows `validate` to dispatch to the appropriate method.

<!-- For example, 
```
@abstract A{T<:Number} begin
	 s::String = "goodbye"
    x::T
    function validate(s, x)
        x >= zero(T) || error("x must be non-negative")
        return (s, x)
    end
end
```
expands to roughly the following code:
```
abstract type A{T<:Number} end

InheritableFields.type_declaration(::A{T}) where {T} = (... the type definition expressions ...)

function InheritableFields.validate(::A{T}, s, x) where {T}
   x >= zero(T) || error("x must be non-negative")
   return (s, x)
end
``` -->