const child_process = require("child_process");

const signals = {
  SIGINT: 2,
  SIGQUIT: 3,
  SIGKILL: 9,
  SIGTERM: 15,
};

async function exec(command, args) {
  const stdoutChunks = [];
  const stderrChunks = [];

  const subprocess = child_process.spawn(command, args, {
    stdio: ["ignore", "pipe", "pipe"],
    cwd: __dirname,
  });

  subprocess.stdout.on("data", chunk => {
    stdoutChunks.push(chunk);
  });

  subprocess.stderr.on("data", chunk => {
    stderrChunks.push(chunk);
  });

  return await new Promise((resolve, reject) => {
    subprocess.on("error", err => {
      reject(err);
    });

    subprocess.on("close", (exitCode, signal) => {
      const stdout = Buffer.concat(stdoutChunks).toString("utf8");
      const stderr = Buffer.concat(stderrChunks).toString("utf8");

      let code = exitCode ?? 1;
      if (signals[signal]) {
        code = signals[signal] + 128;
      }

      resolve({ code, stdout, stderr });
    });
  });
}

exports.exec = exec;
