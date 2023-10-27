

var dgram = require('dgram');

var socket = new dgram.createSocket('udp4');


let i = 0;

socket.on('message', function(data) {
	console.log('received: ' + String(data)); //recieved
});

socket.on('close', function() {
	console.log('S closed');
});



socket.bind(() => { 
	setInterval(() => {
		const r  = (i++ % 3);
		r == 0 && socket.send(`test 192\n`, 2000, '192.168.88.9');
		r == 1 && socket.send(`test 224\n`, 2000, '224.0.0.1');
		r == 2 && socket.send(`test 235\n`, 2000, '235.1.1.1');
	}, 500)	
});