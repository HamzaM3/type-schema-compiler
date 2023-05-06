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