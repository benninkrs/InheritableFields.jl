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
Abstract types can be subtyped:
```
@abstract B{T} <: A{T} begin
	i::Int
end
```
Use `@mutable` or `@immutable` to create mutable or immutable `struct` type that inherits fields of all its `@abstract` supertypes and can introduce additional fields:
```
@immutable C{T} <: B{T} begin
	b::Bool
end
```
In this example, instances of type `C{T}` will have fields `s::String`, `x::T`, `i::Int`, and `b::bool`, in that order.

To achieve an effective hierarchy of concrete types, say `Person >: Employee :> Salaried`, first create a hierarchy of `@abstract` types, e.g. `AbstractPerson :> AbstractEmployee >: AbstractSalaried`.  Then, define methods that dispatch on these abstract types.  Finally, define a concrete type for each abstract type in the hierarachy (e.g. `@mutable Person <: AbstractPerson begin end`).

<!-- c = C("hello", 1.2, -6, true)
```
`c` is of type `C{Float64}` with fields `s = "hello"`, `x = 1.2`, `i = -6`, `b = true` in that order.  The general principle is that the fields of the concrete type and all `@abstract` supertypes are concatenated in order from the most abstract supertype to the least abstract.  For this reason, the fieldnames of a subtype must be distinct from those of its aancestor types. -->


## Advanced Usage

### Keyword Construction and Default Values

Besides the default constructor, a keyword-based constructor is defined for each `@mutable` or `@immutable` type.  This allows field values to be specified in any order using the field names as keywords:
```
c = C(; i = -6, x = 1.2, b = true, s = "hello")  # == C{Float64}("goodbye", 1.2, -6, true)
```
Besides providing increased readibility and robustness to field order, the keyword constructor allows the use of default values. Any field declaration may optionally include a default value by expressing it as an assignment:
```
@abstract A{T<:Number}
begin
	s::String = "goodbye"
	x::T
end
```
The default value of a field is used when the keyword constructor is invoked without specifying a value for that field:
```
c = C(; i = -6; b = true; x = 1.2)  # == C{Float64}("goodbye", 1.2, -6, true)
```

### Validation of Construction Values

Within the body of an `@abstract`, `@mutable`, or `@immutable` type definition, one can implement a special method to validate construction values for the fields introduced by that type. For example, suppose type `A` requires `x` to be nonnegative.  This can be enforced as follows:
```
@abstract A{T<:Number} begin
	 s::String = "goodbye"
    x::T
    function validate(s, x)
        x >= zero(T) || error("x must be non-negative")
        return (s, x)
    end
end

# ... definitions of B and C above ...

c = C(; i = -6; b = true; x = -1.2)
ERROR: x must be non-negative
```
The constructors that are automatically created for a `@mutable` or `@mmutable` type pass the input arguments associated with each type in the hierarchy to the provided validation function.

A validation method should be defined within the type body, be named `validate`, and have the same parameterization and signature that a default constructor would.  It should either return valid arguments for a default constructor or throw an exception.  


## Hygiene

Type parameters propagate from subtypes to supertypes as one would expect, regardless of their formal names. Similarly, type definitions will be evaluated correctly even if defined in different modules.

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

## Limitations

`validate` methods should only be defined within the bodies of `@abstract`, `@mutable`, or `@immutable` type definitions.
-->



## Implementation

The  `@abstract` macro does the following
 1. Define the specified type as `abstract type`.
 2. Store the definition of the type's signature and fields (if any).
 3. Define the `validate` method for the type (if provided).

 The `@mutable` and `@immutable` macros are similar, except the first step is:
  1. Define the specified type as `mutable struct` or `struct`, along with (i) an argument-validtating default constructor, and (ii) an argument-validating keyword constructor. 

The type definitions are not stored in a lookup table, but as methods for a function called `type_declaration`.  Given a type, this function returns the macro-defined type definition.

```
@abstract A{T<:Number} begin
	 s::String = "goodbye"
    x::T
    function validate(s, x)
	 	println("s = $s, x = $x")
        x >= zero(T) || error("x must be non-negative")
        return (s, x)
    end
end
```
expands to roughly the following code:
```
abstract type A{T<:Number} end

InheritableFields.type_declaration(::A{T}) where {T} = (... the type definition expressions ...)

function InheritableFields.validate(::A{T}) where {T}
	(... body of the provided function ...)
end
```