

let ((>::),
     (>:::)) = OUnit.((>::),(>:::))

let (=~) = OUnit.assert_equal


let test_eq x y  = 
    Bsb_regex.global_substitute "\\${bsb:\\([-a-zA-Z0-9]+\\)}"
        (fun _ groups -> 
            match groups with 
            | x::xs -> x 
            | _ -> assert false 
        ) x =~ y 


let suites = 
    __FILE__ 
    >:::
    [
        __LOC__ >:: begin fun _ -> 
        test_eq 
        {| hi hi hi ${bsb:name}
        ${bsb:x}
        ${bsb:u}
        |}        
        {| hi hi hi name
        x
        u
        |}
    end;
    __LOC__ >:: begin  fun _ ->
    test_eq  "xx" "xx";
    test_eq "${bsb:x}" "x";
    test_eq "a${bsb:x}" "ax";
    
    end;

    __LOC__ >:: begin fun _ ->
        test_eq "${bsb:x}x" "xx"
    end;

    __LOC__ >:: begin fun _ -> 
        test_eq {|
{
  "name": "${bsb:name}",
  "version": "${bsb:proj-version}",
  "sources": [
    "src"
  ],
  "reason" : { "react-jsx" : true},
  "bs-dependencies" : [
      // add your bs-dependencies here 
  ]
}
|} {|
{
  "name": "name",
  "version": "proj-version",
  "sources": [
    "src"
  ],
  "reason" : { "react-jsx" : true},
  "bs-dependencies" : [
      // add your bs-dependencies here 
  ]
}
|}
    end

    ;
    __LOC__ >:: begin fun _ -> 
    test_eq {|
{
  "name": "${bsb:name}",
  "version": "${bsb:proj-version}",
  "scripts": {
    "clean": "bsb -clean",
    "clean:all": "bsb -clean-world",
    "build": "bsb",
    "build:all": "bsb -make-world",
    "watch": "bsb -w",
  },
  "keywords": [
    "Bucklescript"
  ],
  "license": "MIT",
  "devDependencies": {
    "bs-platform": "${bsb:bs-version}"
  }
}
|} {|
{
  "name": "name",
  "version": "proj-version",
  "scripts": {
    "clean": "bsb -clean",
    "clean:all": "bsb -clean-world",
    "build": "bsb",
    "build:all": "bsb -make-world",
    "watch": "bsb -w",
  },
  "keywords": [
    "Bucklescript"
  ],
  "license": "MIT",
  "devDependencies": {
    "bs-platform": "bs-version"
  }
}
|}
    end;
    __LOC__ >:: begin fun _ -> 
    test_eq {|
{
    "version": "0.1.0",
    "command": "${bsb:bsb}",
    "options": {
        "cwd": "${workspaceRoot}"
    },
    "isShellCommand": true,
    "args": [
        "-w"
    ],
    "showOutput": "always",
    "isWatching": true,
    "problemMatcher": {
        "fileLocation": "absolute",
        "owner": "ocaml",
        "watching": {
            "activeOnStart": true,
            "beginsPattern": ">>>> Start compiling",
            "endsPattern": ">>>> Finish compiling"
        },
        "pattern": [
            {
                "regexp": "^File \"(.*)\", line (\\d+)(?:, characters (\\d+)-(\\d+))?:$",
                "file": 1,
                "line": 2,
                "column": 3,
                "endColumn": 4
            },
            {
                "regexp": "^(?:(?:Parse\\s+)?(Warning|[Ee]rror)(?:\\s+\\d+)?:)?\\s+(.*)$",
                "severity": 1,
                "message": 2,
                "loop": true
            }
        ]
    }
}
|} {|
{
    "version": "0.1.0",
    "command": "bsb",
    "options": {
        "cwd": "${workspaceRoot}"
    },
    "isShellCommand": true,
    "args": [
        "-w"
    ],
    "showOutput": "always",
    "isWatching": true,
    "problemMatcher": {
        "fileLocation": "absolute",
        "owner": "ocaml",
        "watching": {
            "activeOnStart": true,
            "beginsPattern": ">>>> Start compiling",
            "endsPattern": ">>>> Finish compiling"
        },
        "pattern": [
            {
                "regexp": "^File \"(.*)\", line (\\d+)(?:, characters (\\d+)-(\\d+))?:$",
                "file": 1,
                "line": 2,
                "column": 3,
                "endColumn": 4
            },
            {
                "regexp": "^(?:(?:Parse\\s+)?(Warning|[Ee]rror)(?:\\s+\\d+)?:)?\\s+(.*)$",
                "severity": 1,
                "message": 2,
                "loop": true
            }
        ]
    }
}
|}
    end
    ]