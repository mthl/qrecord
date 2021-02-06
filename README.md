# qrecord

`defrecord` with qualified field names.

[![Clojars Project](https://clojars.org/fr.reuz/qrecord/latest-version.svg)](https://clojars.org/fr.reuz/qrecord)

## Problem

Using namespaced identifiers is an excellent way to represent information because it allows the accretion of data in aggregates without any risk of conflicting names and makes the meaning associated with those identifiers global. This is the technique used by the [*Resource Description Framework* (RDF)](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/) to share data across the Web.

Clojure has adopted those ideas by providing qualified keywords and symbols literals with some convenient aliasing capabilities. [clojure.spec]( https://clojure.org/about/spec) is a great example of library leveraging the benefits associated with namespaced identifiers by provided global specification to qualified keywords which can then be reused when specifying aggregates. This approach contrasts with the one used in most type systems where the specification of a property depends on the context of its aggregate.

When it comes to performance a common technique in Clojure is to rely on [defrecord](https://clojure.org/reference/datatypes) instead of plain maps to define aggregates. Unfortunately `defrecord` only supports unqualified field names which mean that developers must choose between performance and clean data representation.

## Solution

Destructing already provides a convenient syntax to work with namespaced keywords. In the following example we associate to the binding `a` the value of the `:domain/id` key from the map `m`.

```clojure
(def m {:domain/id 14 :other/id "UV"})

(let [{:keys [domain/id]} m] id)
;;; ⤷ 14
```

What we propose is to have a similar syntax for `defrecord` where methods could refer to fields using the name part of the field identifier.

```clojure
(defrecord Employee [domain/id domain/full-name]
  Object
  (toString [_]
    (str "<< id: " id ", name: " full-name " >>")))

(def alyssa (->Employee 14 "Alyssa P. Hacker"))

(.toString alyssa)
;;; ⤷ "<< id: 14, name: Alyssa P. Hacker >>"
```

While the namespace part is used to access fields using keywords.

```clojure
(:domain/id alyssa)
;;; ⤷ 14

(assoc alyssa :domain/favorite-food "pizza")
;;; ⤷ {:domain/id 14,
;;;    :domain/full-name "Alyssa P. Hacker",
;;;    :domain/favorite-food "pizza"}

(record? alyssa)
;;; ⤷ true
```

This solution provides the performance of records while preserving the clean data representation model of namespaced identifiers.

## Usage

The implementaton of this solution is available as a library in the `qrecord.core` namespace

```clojure
(require '[qrecord.core :as q])

(q/defrecord MyType [domain/x other/y])

(def rec-a (->MyType "a" 7))

(def rec-b (map->MyType {:domain/x "b" 21}))
```

## License

Copyright © 2006-2021 Rich Hickey

Copyright © 2021 Mathieu Lirzin

Distributed under the Eclipse Public License, the same as Clojure.
