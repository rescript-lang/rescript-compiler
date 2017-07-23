

package main

import (
	"os/exec"
	"path"
	"fmt"
	// "path/filepath"
	"os"
)


func checkError(err error){
	if err!= nil {
		fmt.Println("Error:", err.Error())
		panic("Failure")
	}
}

func exists(name string) bool {
    _, err := os.Stat(name)
    return !os.IsNotExist(err)
}
const PACKAGEJSON= "package.json"

func findPackageJSON(cwd string)string{
	if exists(PACKAGEJSON) {
		return cwd
	}
	return findPackageJSON(path.Join(cwd,".."))
}
func main(){
	cwd,err := os.Getwd()
	packageDir := findPackageJSON(cwd)
	fmt.Println("PackageDir", packageDir)
	checkError(err)
	workDir := path.Join(packageDir,"ocaml")

	cmd:=exec.Command("git", 
		"archive", "HEAD","-o",
		path.Join(packageDir, "vendor","ocaml.tar.gz"),
		)
	cmd.Dir = workDir
	output, ok := cmd.CombinedOutput()
	checkError(ok)
	fmt.Println(output)
}