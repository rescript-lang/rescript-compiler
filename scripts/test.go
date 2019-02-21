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
	"sync"
	"time"
	"runtime"
)



func checkError(err error, theme ...string) {
	if err != nil {
		log.Fatalf("Error in theme:%v\n====\n%s\n====\n", theme, err.Error())
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


var ninja  string


var cmd = exec.Command

func checkFileExist(f string) bool{
	if _, err := os.Stat(f); os.IsNotExist(err){
		return false
	}
	return true
}
// Avoid rebuilding OCaml again
func init() {
	vendorOCamlPath, _ := filepath.Abs(filepath.Join(".", "vendor", "ocaml", "bin"))
	os.Setenv("PATH",
		vendorOCamlPath+string(os.PathListSeparator)+os.Getenv("PATH"))
	var extension string 	

	if runtime.GOOS == "linux" {
		extension = "linux"
	} else if runtime.GOOS == "darwin" {
		extension = "darwin"
	} else {
		log.Fatalf("not supported platform for testing")
	}
	vendored :=filepath.Join("vendor","ninja", "snapshot", "ninja."+extension)
	if checkFileExist(vendored){
		ninja = vendored
	} else if  new:= filepath.Join("lib","ninja.exe"); checkFileExist(new) {
		ninja = new
	} else {
		fmt.Println("ninja could not be configured")
		os.Exit(2)

	}
}

func bsbInDir(builddir, dir string) {
	script:= "input.js"
	destDir := filepath.Join(builddir, dir)

	if !checkFileExist(path.Join(destDir,script)){
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
	installGlobal := flag.Bool("install-global", false, "don't install global")
	ounitTest := flag.Bool("ounit", false, "don't do ounit test")
	mochaTest := flag.Bool("mocha", false, "don't run mocha")
	themeTest := flag.Bool("theme", false, "no bsb theme test")
	bsbTest := flag.Bool("bsb", false, "no bsb test")
	all := flag.Bool("all",false,"test all")
	// disableAll := flag.Bool("disable-all", false, "disable all tets")
	flag.Parse()
	if *all {
		*installGlobal = true
		*ounitTest = true
		*mochaTest = true
		*themeTest = true
		*bsbTest = true
	}
	output, _ := cmd("which", "ocaml").CombinedOutput()
	fmt.Println("OCaml:", string(output))
	if *ounitTest {
		btest := cmd("make", "-C", "jscomp/bin", "test")
		btest.Stdout = os.Stdout
		btest.Stderr = os.Stderr 
		berror := btest.Run()
		if berror != nil {
			os.Exit(2)
		}
	}
	if *mochaTest {
	
		make := cmd("sh", "-c", "mocha jscomp/test/**/*test.js") 
		make.Stdout = os.Stdout
		make.Stderr = os.Stderr
		error := make.Run()
		if error != nil {
			fmt.Println(error)
			os.Exit(2)
		}
	}

	if *installGlobal {
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
	
	if *themeTest {
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
	if *bsbTest {
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
