module.exports = {
  files: {
    javascripts: {
      joinTo: "js/app.js"
    },
    stylesheets: {
      joinTo: "css/style.css"
    }
  },
  plugins: {
    elm: {
      "exposed-modules": ["Main"],
      renderErrors: true,
      parameters: ["--yes", "--warn"]
    }
  },
  overrides: {
    production: {
      plugins: {
        elm: {
          renderErrors: false,
          parameters: ["--yes", "--warn"]
        }
      }
    }
  }
};
