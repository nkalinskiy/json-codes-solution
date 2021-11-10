# Json Codecs
Your task is to implement a json parser module using a typeclass pattern.
The program should be able to parse to/from Json:
* strings
* integers
* floating point numbers with double precision
* lists
* user types
### Json
Json can be represented by string, int, double, array or 
a composite json object
### JsonWriter
This typeclass can accept a value of any type and transform 
into Json object. Also it has a typeclass instance summoner 
and a syntax extension to ease transformations. 
If there is no writer in scope for a type an instance of a parent type should be used.
### JsonReader
This typeclass can accept a json and return an object constructed from it as Right part.
If it encounters errors, it accumulates them and return all in the end as Left part.

There are two types of errors: 
* `WrongType` - reader found field with the corresponding name, but it was of other type than expected by object's constructor
* `AbsentField` - reader could not find field with the corresponding name in json. 
If it encounters this error in a list of elements, it should record field as "list `key`, element `id`: element `field`". 
An example is provided in tests. If Json is only a JsonArray, then only element's `id` and `field` should be stated in error.
It there is any nesting of objects, parent object key name should be prepended to every error `field`
* You can define your own error adt or error fields if you think that proposed ones are not enough to accumulate all nested errors appropriately. 
But do not forget to show custom errors in tests.

You can use any syntax for `Either`/`Validated` or any other types that you will find useful in `cats` library.
Just import `cats.data.Validated`/`cats.syntax.either._`/`cats.syntax.validated._` and see where it gets you.
### Useful resources
* http://typelevel.org/cats/typeclasses.html
* http://typelevel.org/cats/datatypes/validated.html
* http://eed3si9n.com/herding-cats/
