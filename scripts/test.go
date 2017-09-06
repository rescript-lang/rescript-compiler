package main

import (
	"path"
	"flag"
	"fmt"
	"io/ioutil"
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
	err error
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
func checkError(err error, theme ...string) {
	if err != nil {
		log.Fatalf("Error:%v\n====\n%s\n====\n", theme, err.Error())
	}
}

func testTheme(theme string) {
	fmt.Println("Removing", theme)
	os.RemoveAll(theme)
	cmd := exec.Command("bsb", "-theme", theme, "-init", theme)
	output, err := cmd.CombinedOutput()

	fmt.Println(string(output))
	checkError(err, theme)

	fmt.Println("Start to install", theme)
	cmd = exec.Command("npm", "install")
	cmd.Dir = theme
	output, err = cmd.CombinedOutput()
	fmt.Println(string(output))
	checkError(err, theme)

	fmt.Println("Started to build ", theme)
	cmd2 := exec.Command("npm", "run", "build")
	cmd2.Dir = theme
	output2, err := cmd2.CombinedOutput()
	fmt.Println(string(output2))
	checkError(err, theme)

	os.RemoveAll(theme)
	fmt.Println("Finish building", theme)
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

// Avoid rebuilding OCaml again
func init() {
	vendorOCamlPath, _ := filepath.Abs(filepath.Join(".", "vendor", "ocaml", "bin"))
	os.Setenv("PATH",
		vendorOCamlPath+string(os.PathListSeparator)+os.Getenv("PATH"))
}
func fatalError(err error) {
	if err != nil {
		panic(err)
	}
}
func bsbInDir(builddir, dir string) {
	script:= "input.js"
	destDir := filepath.Join(builddir, dir)

	if _, err := os.Stat(path.Join(destDir,script)); os.IsNotExist(err){
		fmt.Println("Warn:", dir, "does not have input.js")
		return
	}
	c := cmd("node", script)	
	c.Dir = destDir	
	out, err := c.CombinedOutput()

	if err != nil {
		fmt.Println("failed in ", dir)
		outS := string(out)
		fmt.Println(outS)
		fmt.Println(err)
		os.Exit(2)
	}
	fmt.Println(string(out))
	fmt.Println("success in ", dir)

}

func main() {
	noInstallGlobal := flag.Bool("no-install-global", false, "don't install global")
	noOunitTest := flag.Bool("no-ounit", false, "don't do ounit test")
	noMochaTest := flag.Bool("no-mocha", false, "don't run mocha")
	noThemeTest := flag.Bool("no-theme", false, "no bsb theme test")
	noBsbTest := flag.Bool("no-bsb", false, "no bsb test")
	// disableAll := flag.Bool("disable-all", false, "disable all tets")
	flag.Parse()

	output, _ := cmd("which", "ocaml").CombinedOutput()
	fmt.Println("OCaml:", string(output))
	if !*noOunitTest {
		btest := cmd("make", "-C", "jscomp/bin", "test")
		btest.Stdout = os.Stdout
		btest.Stderr = os.Stderr 
		berror := btest.Run()
		if berror != nil {
			os.Exit(2)
		}
	}
	if !*noMochaTest {
		make := cmd("make", "-C", "jscomp", "travis-world-test")
		make.Stdout = os.Stdout
		make.Stderr = os.Stderr
		error := make.Run()
		if error != nil {
			os.Exit(2)
		}
	}

	if !*noInstallGlobal {
		ginstall := cmd("npm", "i", "-g", ".")
		fmt.Println("install bucklescript globally")
		start := time.Now()
		ginstall.Stdout = os.Stdout
		ginstall.Stderr = os.Stderr
		error := ginstall.Run()
		if error != nil {
			log.Fatalf("install failed")
		} else {

			fmt.Println("install finished takes", time.Since(start))
		}
		bsbDir, _ := cmd("bsb", "-where").CombinedOutput()
		fmt.Println("BSBDIR:", string(bsbDir))
	}
	
	if !*noThemeTest {
		var wg sync.WaitGroup
		for _, theme := range []string{
			"basic",
			"basic-reason",
			"generator",
			"minimal",
			"node",
			"react",
		} {
			fmt.Println("Test theme", theme)
			wg.Add(1)
			go (func(theme string) {
				defer wg.Done()
				testTheme(theme)
			})(theme)
		}
		wg.Wait()
	}
	if !*noBsbTest {
		var wg sync.WaitGroup		
		buildTestDir := filepath.Join("jscomp", "build_tests")
		files, err := ioutil.ReadDir(buildTestDir)
		checkError(err)

		for _, file := range files {
			file := file
			wg.Add(1)
			if file.IsDir() {
				 go func(){
					 defer wg.Done()
					 bsbInDir(buildTestDir, file.Name())
				 }()
			}
		}
		wg.Wait()
	}
}
