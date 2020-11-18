open Warp_Types_Client;

let withCredentials = (client, withCredentials) => {
  {...client, withCredentials};
};

let async = (client, async) => {
  {...client, async};
};

let timeout = (client, timeout) => {
  {...client, timeout};
};

let auth = (client, username, password) => {
  {...client, username: Some(username), password: Some(password)};
};

let overrideMimeType = (client, mimeType) => {
  {...client, overrideMimeType: Some(mimeType)};
};