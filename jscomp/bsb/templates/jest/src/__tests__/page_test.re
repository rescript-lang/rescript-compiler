open Jest;
open Expect;

let _ =

describe("Page", () => {
  test("renders", () => {
    let tree = <Page one="Hello world!" two="will and kate!!!" /> |> ReactShallowRenderer.renderWithRenderer;

    expect(tree) |> toMatchSnapshot;
  });
});
