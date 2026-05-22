var dgram = require('dgram');

var socket = new dgram.createSocket('udp4');

let i = 0;

socket.on('message', function (data, info) {
	console.log(info);
	console.log('received: ' + String(data)); //recieved
});

socket.on('close', function () {
	console.log('S closed');
});

// socket.bind({ port: 2000 });

socket.bind(() => { 
	setInterval(() => {
		const r  = (i++ % 4);
		r == 0 && socket.send(`test 192\n`, 2000, '192.168.88.100');
		r == 1 && socket.send(`test 192\n`, 2000, '192.168.88.101');
		r == 2 && socket.send(`test 192\n`, 2000, '192.168.88.135');
		r == 3 && socket.send(`test 192\n`, 2000, '192.168.88.150');
		// r == 1 && socket.send(`test 224\n`, 2000, '224.0.0.1');
		// r == 2 && socket.send(`test 235\n`, 2000, '235.1.1.1');
	}, 500)	
});