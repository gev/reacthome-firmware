

var net = require('net');

var client = new net.Socket();
client.connect(23, '192.168.88.9', function() {
	console.log('Connected');
	// client.write('Hello, server!');
});


let i = 0

client.on('data', function(data) {
	console.log(String(data)); //recieved

  client.write(`test ${i++}\n`);
	// client.destroy(); // kill client after server's response
});

client.on('close', function() {
	console.log('Connection closed');
});