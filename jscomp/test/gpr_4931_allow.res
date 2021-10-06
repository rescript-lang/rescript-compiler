@@config({
  flags : ["-bs-package-output", "es6:jscomp/test:.mjs"]
})

%%raw(`
if(import.meta.hot) {
  import.meta.hot.accept();
}else{
  console.log("ok")
}`)

