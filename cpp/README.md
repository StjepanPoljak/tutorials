# C++ Notes

## Super C

The most important features that build upon C are:

* function overloading
* `auto` keyword
* reference operator
* namespaces
* implicit `typedef` (for `enum`, `struct` and `union`)

Note: To assign class or function to a namespace use:

```cpp
namespace MyNamespace {
	/* class and function definitions */
}
```

Then, you can use it by invoking `MyNamespace::<class/function>` or with `using namespace MyNamespace` (then it will be available in the whole translation unit / source file).

### Mixing C and C++

To compile C++ code in the C-style the following pattern is used:

```cpp
#ifdef __cplusplus
extern "C" {
#endif

/* definitions and declarations */

#ifdef __cplusplus
}
#endif
```

## C++ standard library

The C++ standard library contains:

* **containers** - data structures holding sequences of objects:
  * *sequence containers* - provide accesses to sequences of elements, like arrays
  * *associative containers* - contain key/value pairs
* **algorithms** - counting, searching, sorting and transforming
* **iterators** - connect containers with algorithms

## Storage duration

There are three types of storage duration:

* **automatic** - automatic object is allocated at the beginning of an enclosing code block
* **static** - declared using the `static` or `extern` keyword (variables declared at global or namespace scope are implicitly `static`):
  * *global static variable*
  * *local static variable* - lifetime from declaration at function scope until program exits
  * *static members* - members of a class not associated with a particular instance of a class
* **thread-local** - each thread will have its own copy of the object that has `thread_local` keyword added to `static` or `extern`

* **dynamic** - allocated and deallocated on request (with `new` and `delete` keywords)

## Value categories

### `lvalue`

An `lvalue` has both value and address / storage. Examples:

```cpp
int test = 10;
std::string = "hello";
```

Here, everything on left side is an `lvalue`.

Note: Functions are usually `rvalue`, but can be `lvalue` as in case of:

```cpp
int& func() {
	static int var = 0;
	return var;
}

int main() {
	func() = 20;
}
```

Note: Defining argument as a reference is actually defining it as an `lvalue`. For example:

```cpp
void func(int& arg) {
	/* do something with arg */
}
```

In this case, `func(10)` won't work, but the following code will:

```cpp
int x;
func(x);
```

### `rvalue`

An `rvalue` has only value and no address / storage. Examples:

```cpp
int a = 10;
int b = 20;
std::string = "hello";
int c = a + b;
```

Here, everything on right-hand side is an `rvalue` (even `a + b`, as its storage is only temporary, until passed to `c`).

Note: It is possible to hack in order to have a reference to `rvalue`:

```cpp
const int& a = 10;
```

The `const` keyword creates a temporary, invisible variable holding `10`. You can also pass both `lvalue` and `rvalue` as an argument in this manner:

```cpp
void func(const int& arg) {
	/* do something with arg */
}
```

If you want to pass only `rvalue`, do:

```cpp
void func(int&& arg) {
	/* do something with arg */
}
```

Note: Latter function definition is useful as an optimization hack, and is usually paired up with the former definition, as an overloaded function.

## Expressions

* **expression statement** - an expression followed by a semicolon
* **compound statement** - sequence of statements enclosed by braces `{ }`
* **declaration statements** - introduce identifiers (such as functions, templates, and namespaces)

### Casting

You can cast an `lvalue` to an `rvalue` by using `std::move` (include `utility`). You can perform only two actions on moved-from object (the `lvalue` we casted from): reassign it or destroy it.

## Keywords and identifiers

### `auto`

The `auto` keyword allows compiler to deduce the type of the variable.

### `const`

The `const` keyword can be used to:

* make **function arguments** (pointers and references) read-only
* promise **methods** won't modify current object's state (place `const` keyword after method name and argument list, but before the body brackets)
* disable modification of **member variables** after their initialization

### `constexpr`

Whenever all the information required to compute an expression is present at compile time, the compiler is compelled to do so if that expression is marked with `constexpr`. All `constexpr` expressions are `const` because they're always fixed at runtime.

### `explicit`

Explicit constructors instruct the compiler not to consider the constructor as a means for implicit conversion.

### `mutable`

Permits modification of the class member declared `mutable` even if the containing object is declared `const`.

### `noexcept`

Mark any function with `noexcept` that you are sure cannot throw an exception (syntax same as with `const`). Destructors should be treated as `noexpect`; they should never throw an exception.

Note: You can also use `noexcept(<expression>)` to conditionally mark function as `noexpect` (if and only if `<expression>` evaluates to `true`).

### `override`

If you want to override a virtual method, then, within the implementation, add the `override` keyword to the method's declaration (after argument list).

### `virtual`

If you want to permit a derived class to override a base class's methods, you use the `virtual` keyword. If you want to require a derived class to implement the method, you can append the `=0` suffix to a method definition (such methods are called pure virtual methods);

### `volatile`

The `volatile` keyword tells the compiler that every access made through this expression must be treated as a visible side effect. This means access cannot be optimized out or reordered with another visible side effect (useful in embedded, e.g. where writing a value to a variable triggers an event on a physical device).

## Casting

Named conversions are language features that explicitly convert one type into another type. The C++ syntax for casting is:

```
named-conversion<desired_type>(object)
```

### `const_cast` 

The `const_cast` discards the `const` modifier, allowing the modification of `const` values. You can also use `const_cast` to remove the `volatile` modifier.

### `static_cast`

The `static_cast` reverses a well-defined implicit conversion, such as an integer type to another integer type. One use-case is from `void*` to `short*`:

```cpp
void* some_pointer;
auto short_pointer = static_cast<short*>(some_pointer);
```

### `reinterpret_cast`

The `reinterpret_cast` allows converting from memory adresses to specific types; it does not ensure correctness, however.

```cpp
auto long_pointer = reinterpret_cast<long*>(0x1000)
```

### `narrow_cast`

It is very easy to implement `narrow_cast` which throws a runtime exception if data loss during `static_cast` is detected:

```cpp
template <typename T, typename U>
T narrow_cast(U value) {
	const auto converted = static_cast<T>(value);
	const auto reverse = static_cast<U>(converted);
	if (value != reverse)
		throw std::runtime_error{ "Narrowing detected.");
	return converted;
}
```

## Exception handling

### Standard exception classes

You can find good information on except class [here](https://en.cppreference.com/w/cpp/header/stdexcept).

### Try-catch

To throw an exception, just use, e.g.:

```cpp
throw std::runtime_error { "Error message" };
```

To catch exception:

```cpp
try {
	/* function that throws exception */
} catch (std::exception &ex) {
	/* handle any exception */
}
```

You can always narrow down handled exceptions, so instead of `std::exception`, you can use, e.g. `std::runtime_error`. Also, you can retrieve the error message by using `what()` method in , for example:

```cpp
try {
	throw std::runtime_error { "Error message" };
} catch (std::runtime_error &ex) {
	cout << "Error: " << ex.what();
}
```

## Constructors

### List of initializers

You can use the following semantics to initialize variables in a constructor:

```cpp
class Point {
	Point(int x, int y): x{ this->x }, y{ this->y } { };

	int x;
	int y;
};
```

### Copy

The copy constructor creates a copy of an object and assigns it to a brand-new object. It is invoked when passing the object to function by value, or by calling the copy constructor explicitely:

```cpp
SomeObject someObjectOriginal;
SomeObject someObjectCopy { someObjectOriginal };
```

The usual way to define a copy constructor is:

```cpp
class SomeObject {
	SomeObject(const SomeObject& other);
};
```

Note: In this constructor, you should copy all member variables, etc. This can be mostly accomplished by using list of initializers.

Note: You can also overload the `=` operator as in the following:

```cpp
class SomeObject {
	SomeObject& operator=(const SomeObject& other) {

		if (this == &other) return *this;

		/* copy member variables here */

		return *this;
	}
};
```

Note: The default copy constructor will just invoke copy constructors on all member elements. This behaviour can be suppressed with the use of `delete` keyword:

```cpp
class SomeObject {
	SomeObject(const SomeObject& other) = delete;
	SomeObject& operator=(const SomeObject& other) = delete;
};
```

#### Copy guidelines

* **correctness** - ensure that class invariants are maintained
* **independence** - the original and the copy shouldn't alter each other's state
* **equivalence** - the original and the copy should be equal

### Move

The move constructor syntax is similar to the copy constructor, except the fact that `rvalue` references are used instead of `lvalue`:

```cpp
class SomeObject {
	SomeObject(SomeObject&& other) noexcept;

	SomeObject& operator=(SomeObject&& other) noexcept {
		if (this = &other) return *this;

		/* assign member variables */

		return *this;
	}
};
```

### Notes

Note: Depending on what you define: destructor, copy constructor, move assignment, move constructor or move assignment, compiler will generate some (according to some rules). This is however discouraged, and any modifications to these semantics should explicitely define constructors / destructors along with `delete` on the ones not intended for use. 

Note: You need `default` keyword if you want both a default constructor and a non-default constructor.

## Patterns

### Initialization

Variables should be initialized with the bracket syntax, as it is the safest (won't generate garbage value, and also doesn't permit narrowing conversions):

```cpp
int x {};
float y { 5.0 };
int z[] { 1, 2, 3 };
```

Note: `x` will be initialized to `0`.

### Comma operator

Allows several expressions separated by commas to be evaluated within a larger expression, from left to right (with the rightmost expression being the return value).

### Structured binding declaration (C++17)

You can do primitive pattern matching, e.g. in error handling by using `return { data, success }` in a function (here: `function()`), and then retrieveng it with `auto [ data, success ] = function();`.

### Type aliasing

A type alias defines a name that refers to a previously defined name. Usage:

```cpp
using <type-alias> = <type-id>;
```

### Polymorphism

Polymorphic code is code you write once and can reuse with different types.

#### Runtime

An interface is a shared boundary that contains no data or code. An implementation is code or data that declares support for an interface. To create an interface, create a class with a default virtual destructor and pure virtual methods, e.g.:

```cpp
class SomeInterface {
	virtual ~SomeInterface() = default;
	virtual void someMethod() = 0;
};
```

Note: You can only deal with pointers or references to interfaces. There are two ways to set member variables (that are actually interfaces):

* **constructor injection** - use an interface reference (as references cannot be reseated, they won't change for the lifetime of the object)

* **property injection** - use a method to set a pointer member (allows to change the object to which the member points)

#### Compile-time

Declare templates with a template prefix, which consists of the keyword `template` followed by angle brackets `< >`. You can define class with template:

```cpp
template <typename T, typename U>
class SomeClass {
	/* use T and U as types here */
};
```

Note: Instantiate the above class by using, e.g.:

```cpp
SomeClass<int, float> someClass;
```

You can also define functions with template:

```cpp
template <typename T, typename U>
T someFunction (U arg) {
	/* function body */
}
```

Note: You can use non-type template parameters as stand-ins for some yet-to-be-specified value. They can be:

* an integral type
* an lvalue reference type
* a pointer type
* a `std::nullptr_t` (type of `nullptr`)
* an `enum class`

Example for a template class accepting arrays of specific size:

```cpp
template <typename T, size_t arr_size, size_t index>
T& get (T(&arr)[arr_size]) {
	if (index >= arr_size)
		throw std::out_of_range{ "Out of bounds" };
	return arr[index];
}
```

Note: You can also use *variadic_templates*:

```cpp
template <typename T, typename... Arguments>
T test(Arguments ... arguments) {
	/* some code */
}
```

##### Concepts

Concepts won't be available until C++20, so `static_assert` is a compile-time evaluation alternative in C++17. The syntax is:

```cpp
static_assert(<boolean-expression>, <optional-message>);
```

You can find type traits to use as `<boolean-expression>` [here](https://en.cppreference.com/w/cpp/types#Type_traits). 

Note: A related family of type support is available in the `<limits>` header, which allows querying verious properties of arithmetic types ([here](https://en.cppreference.com/w/cpp/types/numeric_limits).

### Future / promise

This is equivalent to conditional variable wait (consumer) and signal (producer), respectively. See examples for use with `std::async` and `std::thread`.

### Overloading operator `new`

By default, operator `new` will allocate memory on the free store (i.e. the heap) to make space for dynamic objects. This behaviour can be tweaked by overloading the following four operators from the `<new>` header:

```cpp
void* operator new(size_t);
void operator delete(void*);
void* operator new[](size_t);
void operator delete[](void*);
```

Note: On issues with allocation, the `std::bad_alloc()` exception should be thrown.

Note: The preferred C++ way to deal with "raw bytes" is the `std::byte` type from `<cstddef>` header.

#### Placement operators

Using placement operators, you can manually construct objects in arbitrary memory (without overriding all free store allocations):

```cpp
void* operator new(size_t, void*);
void operator delete(size_t void*);
void* operator new[](void*, void*);
void operator delete[](void*, void*);
```

For example:

```cpp
class SomeClass {
	// ...
}

int main() {
	std::byte data[sizeof(SomeClass) * 2];
	SomeClass firstInstance = new(&data[0]) SomeClass{};
	SomeClass secondInstance = new(&data[sizeof(SomeClass)]) SomeClass{};
	firstInstance->~SomeClass();
	secondInstance->~SomeClass();
}
```

Note: You cannot use `delete` to release the resulting dynamic objects. You must call the object's destructor directly and only once.

## Standard Template Library (STL)
