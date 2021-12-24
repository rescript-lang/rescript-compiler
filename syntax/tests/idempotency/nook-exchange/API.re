let makeAuthenticatedPostRequest = (~url, ~bodyJson, ~sessionId) => {
  Fetch.fetchWithInit(
    url,
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=
        Fetch.BodyInit.make(
          Js.Json.stringify(Json.Encode.object_(bodyJson)),
        ),
      ~headers=
        Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization":
            "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
        }),
      ~credentials=Include,
      ~mode=CORS,
      (),
    ),
  );
};

let setItemStatus = (~userId, ~sessionId, ~itemId, ~variant, ~status) => {
  let%Repromise.Js responseResult =
    makeAuthenticatedPostRequest(
      ~url=
        Constants.apiUrl
        ++ "/@me/items/"
        ++ string_of_int(itemId)
        ++ "/"
        ++ string_of_int(variant)
        ++ "/status",
      ~bodyJson=[
        ("status", Json.Encode.int(User.itemStatusToJs(status))),
        ("userId", Json.Encode.string(userId)),
      ],
      ~sessionId,
    );
  Promise.resolved(responseResult);
};

let setItemStatusBatch = (~sessionId, ~items: array((int, int)), ~status) => {
  let url = Constants.apiUrl ++ "/@me/items/batch/status";
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.(
                object_([
                  ("items", array(tuple2(int, int), items)),
                  ("status", int(User.itemStatusToJs(status))),
                ])
              ),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let setItemNote = (~userId, ~sessionId, ~itemId, ~variant, ~note) => {
  let%Repromise.Js responseResult =
    makeAuthenticatedPostRequest(
      ~url=
        Constants.apiUrl
        ++ "/@me/items/"
        ++ string_of_int(itemId)
        ++ "/"
        ++ string_of_int(variant)
        ++ "/note",
      ~bodyJson=[
        ("note", Json.Encode.string(note)),
        ("userId", Json.Encode.string(userId)),
      ],
      ~sessionId,
    );
  Promise.resolved(responseResult);
};

let setItemPriority = (~sessionId, ~itemId, ~variant, ~isPriority) => {
  let%Repromise.Js responseResult =
    makeAuthenticatedPostRequest(
      ~url=
        Constants.apiUrl
        ++ "/@me/items/"
        ++ string_of_int(itemId)
        ++ "/"
        ++ string_of_int(variant)
        ++ "/priority",
      ~bodyJson=[("isPriority", Json.Encode.bool(isPriority))],
      ~sessionId,
    );
  Promise.resolved(responseResult);
};

let importItems =
    (~sessionId, ~updates: array(((int, int), User.itemStatus))) => {
  let%Repromise.Js responseResult =
    makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++ "/@me/items/import",
      ~bodyJson=[
        (
          "updates",
          updates
          |> Json.Encode.(
               array((((itemId, variant), status)) =>
                 tuple3(
                   int,
                   int,
                   int,
                   (itemId, variant, User.itemStatusToJs(status)),
                 )
               )
             ),
        ),
      ],
      ~sessionId,
    );
  Promise.resolved(responseResult);
};

let removeItem = (~userId, ~sessionId, ~itemId, ~variant) => {
  let url =
    Constants.apiUrl
    ++ "/@me/items/"
    ++ string_of_int(itemId)
    ++ "/"
    ++ string_of_int(variant);
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.object_([("userId", Json.Encode.string(userId))]),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization":
              "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let removeItems = (~sessionId, ~items: array((int, int))) => {
  let url = Constants.apiUrl ++ "/@me/items/batch";
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.(
                object_([("items", array(tuple2(int, int), items))])
              ),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let updateProfileText = (~userId, ~sessionId, ~profileText) => {
  let%Repromise.Js responseResult =
    makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++ "/@me/profileText",
      ~bodyJson=[
        ("text", Json.Encode.string(profileText)),
        ("userId", Json.Encode.string(userId)),
      ],
      ~sessionId,
    );
  Promise.resolved(responseResult);
};

let updateSetting = (~userId, ~sessionId, ~settingKey, ~settingValue) => {
  let url = Constants.apiUrl ++ "/@me/settings";
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Patch,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.object_([
                ("key", Json.Encode.string(settingKey)),
                ("value", settingValue),
                ("userId", Json.Encode.string(userId)),
              ]),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization":
              "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let patchMe =
    (~userId, ~sessionId, ~username, ~newPassword, ~email, ~oldPassword) => {
  open Belt;
  let url = Constants.apiUrl ++ "/@me";
  let%Repromise.JsExn response =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Patch,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Js.Json.object_(
                Js.Dict.fromArray(
                  Array.keepMap(
                    [|
                      Option.map(username, username =>
                        ("username", Js.Json.string(username))
                      ),
                      Option.map(newPassword, newPassword =>
                        ("password", Js.Json.string(newPassword))
                      ),
                      Option.map(email, email =>
                        ("email", Js.Json.string(email))
                      ),
                      Option.map(oldPassword, oldPassword =>
                        ("oldPassword", Js.Json.string(oldPassword))
                      ),
                    |],
                    x =>
                    x
                  ),
                ),
              ),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization":
              "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(response);
};

let getUserLists = (~sessionId) => {
  let url = Constants.apiUrl ++ "/@me/item-lists";
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let createItemList = (~sessionId, ~items: array((int, int))) => {
  let url = Constants.apiUrl ++ "/item-lists";
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.(
                object_([("items", array(tuple2(int, int), items))])
              ),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization":
              "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let cloneItemList = (~sessionId, ~listId) => {
  let url = Constants.apiUrl ++ "/item-lists/" ++ listId ++ "/clone";
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let updateItemList =
    (
      ~sessionId,
      ~listId,
      ~title=?,
      ~items: option(array((int, int)))=?,
      (),
    ) => {
  let url = Constants.apiUrl ++ "/item-lists/" ++ listId;
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Patch,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.(
                object_(
                  Belt.List.keepMap(
                    [
                      title->Belt.Option.map(title =>
                        ("title", string(title))
                      ),
                      items->Belt.Option.map(items =>
                        ("items", array(tuple2(int, int), items))
                      ),
                    ],
                    x =>
                    x
                  ),
                )
              ),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let deleteItemList = (~sessionId, ~listId) => {
  let url = Constants.apiUrl ++ "/item-lists/" ++ listId;
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let getItemList = (~listId: string) => {
  let url = Constants.apiUrl ++ "/item-lists/" ++ listId;
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
          }),
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let followUser = (~userId, ~sessionId) => {
  let%Repromise.JsExn response =
    Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/follow/" ++ userId,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  if (Fetch.Response.status(response) < 300) {
    Promise.resolved(Ok());
  } else {
    let%Repromise.JsExn text = Fetch.Response.text(response);
    Promise.resolved(Error(text));
  };
};

let unfollowUser = (~userId, ~sessionId) => {
  let%Repromise.JsExn response =
    Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/unfollow/" ++ userId,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  if (Fetch.Response.status(response) < 300) {
    Promise.resolved(Ok());
  } else {
    let%Repromise.JsExn text = Fetch.Response.text(response);
    Promise.resolved(Error(text));
  };
};

let getFolloweesItem = (~sessionId, ~itemId) => {
  let%Repromise.JsExn response =
    Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/followees/items/" ++ string_of_int(itemId),
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(response);
};

let connectDiscordAccount = (~sessionId, ~code) => {
  let%Repromise.JsExn response =
    Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/discord",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=
          Fetch.BodyInit.make(
            Js.Json.stringify(
              Json.Encode.object_([("code", Js.Json.string(code))]),
            ),
          ),
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(response);
};

let removeAllItems = (~sessionId) => {
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/items/all",
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};

let deleteAccount = (~sessionId, ~userId) => {
  let%Repromise.Js responseResult =
    Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/" ++ userId,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Content-Type": "application/json",
            "Authorization": "Bearer " ++ sessionId,
          }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    );
  Promise.resolved(responseResult);
};
