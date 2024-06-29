# InheritableFields.jl

Provides a convenient, robust, and performant way to define abstract types with inheritable fields.

This functionality is similar to that of several other packages: [Classes.jl](https://github.com/rjplevin/Classes.jl), [ConcreteAbstractions.jl](https://github.com/tbreloff/ConcreteAbstractions.jl),
[OOPMacro.jl](https://github.com/ipod825/OOPMacro.jl), and [ObjectOriented.jl](https://github.com/Suzhou-Tongyuan/ObjectOriented.jl), and [Inherit.jl](https://github.com/mind6/Inherit.jl).
See also [Mixers.jl](https://github.com/rafaqz/Mixers.jl) and [ReusePatterns.jl](https://github.com/gcalderone/ReusePatterns.jl).

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

c = C("hello", 1.2, -6, true)
```
`c` is of type `C{Float64}` with fields `s = "hello"`, `x = 1.2`, `i = -6`, `b = true` in that order.  The general principle is that the fields of the concrete type and all `@abstract` supertypes are concatenated in order from the most abstract supertype to the least abstract.  For this reason, the fieldnames of a subtype must be distinct from those of its aancestor types.


## Advanced Usage

### Keyword Construction and Default Values

Besides the default constructor, a keyword-based constructor is defined for each `@mutable` or `@immutable` type.  This allows field values to be specified in any order using the field names as keywords:
```
c = C(; i = -6, x = 1.2, b = true, s = "hello")  # == C{Float64}("goodbye", 1.2, -6, true)
```
Besides providing increased readibility and robustness to field order, the keyword constructor allows the use of optional  _default values_ included in the field declarations:
```
@abstract A{T<:Number}
begin
	s::String = "goodbye"
	x::T
end
```
The default value of a field is used when the keyword constructor is invoked without a value for the field's name:
```
c = C(; i = -6; b = true; x = 1.2)  # == C{Float64}("goodbye", 1.2, -6, true)
```

### Validation of Construction Values

Within the body of an `@abstract`, `@mutable`, or `@immutable` type definition, one can implement the function `validate` to validate construction values for the fields introduced by that type. For example, suppose type `A` requires `x` to be nonnegative.  This can be enforced as follows:
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

# ... definitions of B and C above ...

c = C(; i = -6; b = true; x = -1.2)
ERROR: x must be non-negative
```
The `validate` method should simply return valid construction values, and throw a helpful error on invalid construction values.  When a `@mutable` or `@mmutable` type is constructed, the `validate` methods of all ancestor types are called in order from most abstract to least abstract.


## Robustness

Type parameters are carefully propagated from subtypes to supertypes. For example, if `C` were defined as
```
@immutable C{U,S,V} <: B{S}
begin
	b::Bool
end
```
then in an instance of type `C{Any, Complex, Char}`, the field `x` would have type `Complex` because the `S` in `C{U,S,V}` is mapped to `B{S}`, which is mapped to `A{S}`, which is mapped to `x::S`.


## Limitations

`validate` methods should only be defined within the bodies of `@abstract`, `@mutable`, or `@immutable` type definitions.

TType definitions are stored as expressions. Currently, if those expressions are evaluated in a different scope, symbols appearing within them may not be properyl resolved.  This is a work in progress.


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