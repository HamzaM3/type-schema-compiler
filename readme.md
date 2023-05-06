# Type schema compiler

A compiler for an intuitive Javascript type schema definition language. Allows to build type checking schemas easily. For know, only compiles to AJV schemas.

(This software has been written using Haskell)

## Usage

Run:

```sh
npx type-schema-compiler [files]
```

It will produce one js file per schema file that exports AJV schemas ready to compile.

## Example

```
schema status
number
---
schema productStore
{
  products: [{
    name: string,
    stars: number,
    price: number
  }],
  store: string
}
---
schema user
[{
  id: number,
  name: string,
  age: number,
  worth: number,
  friends: [{
    id: number
  }]
}]
```

It will compile into a file of this shape

```js
// schemaList.js
export const status = { type: "number" };
export const productStore = {type: ...};
export const user = {type: ...};
```

You can import them within a parent file and insert them in AJV compiler and move from there

```js
import { status, productStore, user } from "./schemaList";
import Ajv from "ajv";

const ajv = new Ajv();

const statusValidator = ajv.compile(status);
const productValidator = ajv.compile(productStore);
const userValidator = ajv.compile(user);

const data = // .... the data to validate

const valid = productValidator.validate(data);

if (valid) {
  // ...
}
```

## TODO

- [ ] Improve error messaging
- [ ] Improve export system
  - [ ] Add the possibility to have API routes as schema names
  - [ ] Make the compilation process within the export
- [ ] Control output folder
- [ ] Add more library choice
- [ ] Add watch mode
- [ ] Improve the complexity of the parsing algorithm
- [ ] Improve error catching
  - [ ] Reserved keyword
  - [ ] Distinct schema names
- [ ] Improve generality
  - [ ] More general object fields
  - [ ] Add tuples
  - [ ] Add checks

Feel free to contribute and suggest more features.
