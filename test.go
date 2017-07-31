package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

type command struct {
	name string
	task (func() (string, error))
}
type commands []command

func andThen(a, b command) command {
	var result command
	result.name = a.name + " && " + b.name
	result.task = (func() (string, error) {
		output, err := a.task()
		if err != nil {
			return output, err
		}
		output2, err := b.task()
		return output + output2, err
	})
	return result
}

// Output ...
type Output struct {
	name string
	// log  string
	err  error
}

func runCommands(commands commands) {
	size := len(commands)
	output := make(chan Output, size)
	var v sync.WaitGroup
	for _, com := range commands {
		v.Add(1)
		go func(f command) {
			defer v.Done()
			fmt.Println("Running", f.name)
			out, err := f.task()
			fmt.Println(out)
			if err != nil {
				output <- Output{name: f.name, err: err}
			}
			
			fmt.Println("Finished", f.name)
		}(com)
	}

	v.Wait()
	close(output)

	fmt.Println("All commands finished")
	failed := 0
	for x := range output {
		failed++
		fmt.Println("Failed command", x.name, x.err)

	}
	if failed == 0 {
		fmt.Println("All commands successufl")
	}

}

func makeCommand(label string, name string, args ...string) command {
	cmd := exec.Command(name, args...)
	return command{
		name: label,
		task: (func() (string, error) {
			output, err := cmd.CombinedOutput()
			return string(output), err
		}),
	}
}
func commandString(name string) command {
	xs := strings.Fields(name)
	if len(xs) == 0 {
		panic("invalid command" + name)
	}
	return makeCommand(name, xs[0], xs[1:]...)

}
func checkError(err error) {
	if err != nil {
		log.Fatalf("Error: %s", err.Error())
	}
}

func testTheme(theme string) {
	fmt.Println("Removing", theme)
	os.RemoveAll(theme)
	cmd := exec.Command("bsb", "-theme", theme, "-init", theme)
	output, err := cmd.CombinedOutput()

	fmt.Println(string(output))
	checkError(err)

	fmt.Println("Started to build ")
	cmd2 := exec.Command("npm", "run", "build")
	cmd2.Dir = theme
	output2, err := cmd2.CombinedOutput()
	fmt.Println(string(output2))
	checkError(err)
	os.RemoveAll(theme)
}

func runMoCha(wg *sync.WaitGroup) {
	defer wg.Done()

	fmt.Println("make -C jscomp/test/all")
	make := exec.Command("make", "-C", filepath.Join("jscomp", "test"))
	makeOut, err := make.CombinedOutput()
	fmt.Println(string(makeOut))
	checkError(err)
	fmt.Println("Running Mocha tests")
	cmd := exec.Command("mocha", "./jscomp/test/**/*test.js")
	output, err := cmd.CombinedOutput()
	fmt.Println(string(output))
	checkError(err)
	fmt.Println("Running mocha finished")

}

func installGlobal(wg *sync.WaitGroup) {
	defer wg.Done()

	cmd := exec.Command("npm", "install", "-g", ".")

	output, err := cmd.CombinedOutput()
	fmt.Printf(string(output))
	checkError(err)
}

var cmd = exec.Command

func init() {
	vendorOCamlPath, _ := filepath.Abs(filepath.Join(".", "vendor", "ocaml", "bin"))
	os.Setenv("PATH",
		vendorOCamlPath+string(os.PathListSeparator)+os.Getenv("PATH"))
}

func main() {

	// Avoid rebuilding OCaml again
	output, _ := cmd("which", "ocaml").CombinedOutput()
	fmt.Println("OCaml:", string(output))

	// runCommands([]command{
	// 	andThen(commandString(`make -C jscomp/test all`),
	// 		commandString(`mocha jscomp/test/**/*test.js`),
	// 	),
	// 	// commandString(`ls *.json`),
	// 	// andThen(
	// 	commandString(`make -C jscomp travis-world-test`),
	// 	commandString(`npm install -g .`),
	// 	// ),
	// 	// makeCommand("make", "-C", filepath.Join("jscomp","test")),
	// })

	make:=cmd ("make", "-C", "jscomp","travis-world-test")
	make.Stdout = os.Stdout
	make.Stderr = os.Stderr
	error:= make.Run()
	if error!= nil {		
		os.Exit(2)
	}

	ginstall := cmd("npm", "i", "-g", ".")
	fmt.Println("install bucklescript globally")
	start :=time.Now()
	error = ginstall.Run()
	if error != nil {
		log.Fatalf("install failed")
	} else {
		
		fmt.Println("install finished takes", time.Since(start))
	}
	bsbDir, _ := cmd("bsb", "-where").CombinedOutput ()
	fmt.Println("BSBDIR:", string(bsbDir))
	

	
	var wg sync.WaitGroup
	for _, theme := range []string{"basic", "basic-reason", "generator", "minimal","node"} {
		fmt.Println("Test theme", theme)
		wg.Add(1)
		go (func(theme string) {
			defer wg.Done()
			testTheme(theme)
		})(theme)
	}
	wg.Wait()
}
