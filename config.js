var CONFIG = {
  document_title: "Documentation to BuckleScript",
  index: "README.md",
  sidebar_file: "sidebar.md",
  base_url: "https://github.com/bloomberg/ocamlscript/blob/master",
    // base for editing features
};

// **************************
// DON'T EDIT FOLLOWING CODES
// **************************

addConfig(ditto, CONFIG);

function addConfig(obj, conf) {
  Object.keys(conf).forEach(function (key) {
    obj[key] = conf[key];
  });
}

