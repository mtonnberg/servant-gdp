# Servant ❤️ GDP

## In a nutshell
To get more productive and reduce the number of needed tests we can combine [Servant](https://haskell-servant.github.io/) (A web api framework) and [GDP](https://kataskeue.com/gdp.pdf) (Ghosts of Departed Proofs) to allow for quite expressive API declarations. This leads to more correct APIs. [See a full example](https://github.com/mtonnberg/gdp-demo).

```haskell
-- http://localhost/div/10/5 would return 2
type DivWebApi numerator denominator =
  "div"
    :> CaptureNamed (Int ~~ numerator 
                    ::: IsPositive numerator)
    :> CaptureNamed (Int ~~ denominator
                    ::: IsPositive denominator)
    :> Get
         '[JSON]
         ( Int ? IsEqualOrLessThan numerator
         )
```
or
```haskell
-- http://localhost/habitats/savanna/animals/3
-- could return ["lion", "elephant"]
type GetAnimalsForHabitat user habitat pagesize =
  AuthProtect "normalUser"
    :> "habitats"
    :> CaptureNamed (String ~~ habitat ::: IsNonEmpty habitat && IsTrimmed habitat)
    :> "animals"
    :> CaptureNamed (Int ~~ pagesize ::: IsPositive pagesize)
    :> Get
         '[JSON]
         ( ( [String ? IsAValidatedAnimal habitat] ? HasAMaximumLengthOf pagesize
           )
             ::: user `HasAccessToHabitat` habitat
         )
```

See <https://github.com/mtonnberg/gdp-demo> for a full, working example.

## How does it work
API-input is captured as named variables, making the named contexts span the entire request.
This in turn, makes it possible to express a lot of domain knowledge/requirements  in the Servant API type.

## Why does this exist?
To both make the API-capabilities clearer and to make it easier to implement.

It is often quite easy to identify domain rules, invariants and preconditions for the API but hard to capture that knowledge in the types. Instead a lot of domain rules and requirements are hidden in the implementation logic.

Moving information to the types are in line with Knowledge-as-Code, read more about the benefits here: [Knowledge-as-Code](https://carboncloud.com/2020/12/07/tech-knowledge-as-code/)