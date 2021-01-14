open Jest;

describe("Expect", () =>
  Expect.(test("toBe", () =>
            expect(Accounting.dummyTimes2(5)) |> toBe(10)
          ))
);
