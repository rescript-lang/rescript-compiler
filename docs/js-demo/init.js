var clientHash = location.hash;

if (clientHash) {
  myCode1Mirror.setValue(atob(clientHash));
}
start();
