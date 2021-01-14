open Jest;

describe("Expect", () =>
  Expect.(test("toBe", () =>
            expect(true) |> toEqual(true)
          ))
);
