

var dgram = require('dgram');

var socket = new dgram.createSocket('udp4');


let i = 0

socket.on('message', function(data) {
	console.log(String(data)); //recieved
});

socket.on('close', function() {
	console.log('S closed');
});

socket.bind(() => { 
	setInterval(() => {
		socket.send(`test ${i++}\n`, 23, '192.168.88.9');
	}, 1000)	
});