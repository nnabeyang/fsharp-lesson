module MyResult

type MyResult<'T, 'Error> =
  | Ok of 'T
  | Error of 'Error

module MyResult =
  let bind binder input =
    match input with
      | Error e -> Error e
      | Ok x -> binder x

type ResultBuilder() =
  member this.Return(value: 'ok): MyResult<'ok, 'error> = Ok value
  member this.Bind(
    input: MyResult<'okInput, 'error>,
    binder: 'okInput -> MyResult<'okOutput, 'error>
  ): MyResult<'okOutput, 'error> = MyResult.bind binder input
let result = ResultBuilder()
