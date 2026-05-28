var dgram = require('dgram');

var socket = new dgram.createSocket('udp4');

let i = 0;
let n = 0;

socket.on('message', function (data, info) {
	console.log(info);
	n++;
	// console.log('received: ' + String(data)); //recieved
});

socket.on('close', function () {
	console.log('S closed');
});

// socket.bind({ port: 2000 });

socket.bind(() => { 
	setInterval(() => {
		console.log("Send " + i);
		socket.send(`test 224\n`, 2000, '224.0.0.1');
		setTimeout(() => {
			console.log(n);
			n = 0;
		}, 500);
		i++;
	}, 1000)	
});