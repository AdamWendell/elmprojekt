require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root ,{ token: localStorage.getItem('token')} );

app.ports.saveToken.subscribe(function(token) {
    localStorage.setItem('token', token)
})

app.ports.deleteToken.subscribe(function(){
    localStorage.removeItem('token')
})
