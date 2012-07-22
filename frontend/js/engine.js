/*
 * Lot of graphic code is strongly based on Nehe tutorials (www.learningwebgl.com)
 * 
 * hsv2rgb from http://schinckel.net/2012/01/10/hsv-to-rgb-in-javascript/ by schinckel
 */

var players;
    function getShader(gl, id) {
        var shaderScript = document.getElementById(id);
        if (!shaderScript) {
            return null;
        }

        var str = "";
        var k = shaderScript.firstChild;
        while (k) {
            if (k.nodeType == 3) {
                str += k.textContent;
            }
            k = k.nextSibling;
        }

        var shader;
        if (shaderScript.type == "x-shader/x-fragment") {
            shader = gl.createShader(gl.FRAGMENT_SHADER);
        } else if (shaderScript.type == "x-shader/x-vertex") {
            shader = gl.createShader(gl.VERTEX_SHADER);
        } else {
            return null;
        }

	gl.shaderSource(shader, str);
	gl.compileShader(shader);

        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            alert(gl.getShaderInfoLog(shader));
            return null;
        }

        return shader;
    }

var gl;
var positionLocation;
var colorLocation;
var lastTick;
var i=0;
var animation=false;

function init() {
	$('body').keydown(function(event) {
		if(event.which == 37) {
			players[config.nick].leftTurn = true;
		} else if(event.which == 39) {
			players[config.nick].rightTurn = true;
		}	});
	$('body').keyup(function(event) {
		if(event.which == 37) {
			players[config.nick].leftTurn = false;
		} else if(event.which == 39) {
			players[config.nick].rightTurn = false;
		}
	});

	canvas = document.getElementById("lesson01-canvas");
        gl = canvas.getContext("experimental-webgl");
        gl.viewportWidth = canvas.width;
        gl.viewportHeight = canvas.height;


	var fragmentShader = getShader(gl, "2d-fragment-shader");
        var vertexShader = getShader(gl, "2d-vertex-shader");

	gl.backgroundProgram = gl.createProgram();
	gl.attachShader(gl.backgroundProgram, getShader(gl, "background-vertex-shader"));
	gl.attachShader(gl.backgroundProgram, getShader(gl, "background-fragment-shader"));
	gl.linkProgram(gl.backgroundProgram);

        gl.program = gl.createProgram();
        gl.attachShader(gl.program, vertexShader);
        gl.attachShader(gl.program, fragmentShader);
        gl.linkProgram(gl.program);
	gl.useProgram(gl.program);


	gl.positionLocation = gl.getAttribLocation(gl.program, "a_position");
        gl.colorLocation = gl.getAttribLocation(gl.program, "a_color");
	gl.matrixUniform = gl.getUniformLocation(gl.program, "uPMatrix");
	var pmatrix = mat4.create();
	mat4.identity(pmatrix);
	if(canvas.width < canvas.height)
	{
		mat4.scale(pmatrix, [1, canvas.width/canvas.height, 1]);
	}
	else
	{
		mat4.scale(pmatrix, [canvas.height/canvas.width, 1, 1]);
	}
	gl.uniformMatrix4fv(gl.matrixUniform, false, pmatrix);

	gl.enable(gl.LINE_SMOOTH);
	gl.lineWidth(3);

        gl.viewport(0.0, 0.0, canvas.width, canvas.height);
	gl.clearColor(0.0, 0.0, 0.0, 1.0);
	gl.disable(gl.DEPTH_TEST);
	d = new Date();
	lastTick = d.getTime();

	initBackground();
	gl.backgroundMatrix = gl.getUniformLocation(gl.backgroundProgram, "uPMatrix");
	gl.uniformMatrix4fv(gl.backgroundMatrix, false, pmatrix);

	gl.useProgram(gl.program);
}
var backgroundTexture;

function handleLoadedTexture(texture) {
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, texture.image);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.bindTexture(gl.TEXTURE_2D, null);
}

function initBackground() {
	var vertices = [-1, -1, -1, 1, 1, -1, 1, 1];
	var coords   = [0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 1.0];
	gl.useProgram(gl.backgroundProgram);
	gl.backgroundBuffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, gl.backgroundBuffer);
	gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
	gl.backgroundBuffer.itemSize = 2;
	gl.backgroundBuffer.numItems = 4;
	gl.backgroundPosLocation = gl.getAttribLocation(gl.backgroundProgram, "a_position");
	gl.vertexAttribPointer(gl.backgroundPosLocation, 2, gl.FLOAT, false, 0, 0);
	gl.enableVertexAttribArray(gl.backgroundPosLocation);
           backgroundTexture = gl.createTexture();
           backgroundTexture.image = new Image();
           backgroundTexture.image.onload = function() {
           handleLoadedTexture(backgroundTexture);
           tick();
       }

        backgroundTexture.image.src = "img/Seattle_pic.jpg";	
	gl.coordsBuffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, gl.coordsBuffer);
	gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(coords), gl.STATIC_DRAW);
	gl.coordsBuffer.itemSize = 2;
	gl.coordsBuffer.numItems = 4;
	gl.coordsLocation = gl.getAttribLocation(gl.backgroundProgram, "a_coords");
	gl.vertexAttribPointer(gl.coordsLocation, 2, gl.FLOAT, false, 0, 0);
	gl.enableVertexAttribArray(gl.coordsLocation);
	
	gl.samplerUniform = gl.getUniformLocation(gl.backgroundProgram, "uSampler");
}

function drawBackground() {
	gl.useProgram(gl.backgroundProgram);
	gl.bindBuffer(gl.ARRAY_BUFFER, gl.backgroundBuffer);
	gl.vertexAttribPointer(gl.positionLocation, 2, gl.FLOAT, false, 0, 0);
	gl.bindBuffer(gl.ARRAY_BUFFER, gl.coordsBuffer);
	gl.vertexAttribPointer(gl.coordsLocation, 2, gl.FLOAT, false, 0, 0);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, backgroundTexture);
        gl.uniform1i(gl.samplerUniform, 0);
	gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

	gl.useProgram(gl.program);
	gl.bindTexture(gl.TEXTURE_2D, null);
}

function hsv(h, s, v)
{
   this.hue = h;
   this.sat = s;
   this.val = v;
}

var hsv2rgb = function(hsv) {
  var h = hsv.hue, s = hsv.sat, v = hsv.val;
  var rgb, i, data = [];
  if (s === 0) {
    rgb = [v,v,v];
  } else {
    h = h / 60;
    i = Math.floor(h);
    data = [v*(1-s), v*(1-s*(h-i)), v*(1-s*(1-(h-i)))];
    switch(i) {
      case 0:
        rgb = [v, data[2], data[0]];
        break;
      case 1:
        rgb = [data[1], v, data[0]];
        break;
      case 2:
        rgb = [data[0], v, data[2]];
        break;
      case 3:
        rgb = [data[0], data[1], v];
        break;
      case 4:
        rgb = [data[2], data[0], v];
        break;
      default:
        rgb = [v, data[0], data[1]];
        break;
    }
  }
  return rgb;
};

function rgbtostring(rgb) {
   return '#' + rgb.map(function(x){
       return ("0" + Math.round(x*255).toString(16)).slice(-2);
   }).join('');
}

function pointFromServer(nick, point, direction) 
{
	players[nick].addPoint(point, direction);
};

function drawTrace(plr) {
	var n = plr.server_trace.length + plr.temp_trace.length;
	var vertices = new Array(2*n);
	var colors = new Array(4*n);
	//var col = rgb(hsv2rgb(new hsv(180, 1, 1)));
	//var col = new color(0, 1, 0, 1);
	var i=0;
	for(i=0; i<plr.server_trace.length; ++i) 
	{
		vertices[2*i] = plr.server_trace[i].x;
		vertices[2*i+1] = plr.server_trace[i].y;
		var hue = Math.sin(4*i/360)*(plr.max_hue - plr.min_hue)/2 + plr.min_hue;
		
		col = rgb(hsv2rgb(new hsv(hue, 1, 1)));
		colors[4*i]   = col.r;
		colors[4*i+1] = col.g;
		colors[4*i+2] = col.b;
		colors[4*i+3] = col.a;
	}

	for(; i<n; ++i)
	{
		j = i - plr.server_trace.length;
		vertices[2*i] = plr.temp_trace[j].x;
		vertices[2*i+1] = plr.temp_trace[j].y;
		var hue = Math.sin(4*i/360)*(plr.max_hue - plr.min_hue)/2 + plr.min_hue;
		
		col = rgb(hsv2rgb(new hsv(hue, 1, 1)));
		colors[4*i]   = col.r;
		colors[4*i+1] = col.g;
		colors[4*i+2] = col.b;
		colors[4*i+3] = col.a;
	}
	buffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
	gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
	buffer.itemSize = 2;
	buffer.numItems = n;
	gl.vertexAttribPointer(gl.positionLocation, 2, gl.FLOAT, false, 0, 0);
	gl.enableVertexAttribArray(gl.positionLocation);

	
	colorBuffer = gl.createBuffer();
	gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
	gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(colors), gl.STATIC_DRAW);
	buffer.itemSize = 4;
	buffer.numItems = n;
	gl.vertexAttribPointer(gl.colorLocation, 4, gl.FLOAT, false, 0, 0);
	gl.enableVertexAttribArray(gl.colorLocation);
	gl.drawArrays(gl.LINE_STRIP, 0, n);
}

function tick() {
	d = new Date();
	ntime = d.getTime();
	time = (ntime - lastTick)/1000;
	lastTick = ntime;

	requestAnimFrame(tick);
	gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
	drawBackground();
	for(var xplr in players) {
		if(animation) players[xplr].step();
		drawTrace(players[xplr]);
	}
}

function rgb(arr) 
{
	return new color(arr[0], arr[1], arr[2], 1);
}

function color(r,g,b,a)
{
	this.r=r;
	this.g=g;
	this.b=b;
	this.a=a;
}

function point(x,y)
{
	this.x=x;
	this.y=y;
};

function player( pos, dir) {
	this.color = new color(0,1,0,1);
	this.position = pos;
	this.direction = dir;
	this.server_trace = [ new point(this.position.x, this.position.y)];
	this.temp_trace = [ new point(this.position.x, this.position.y) ];
	this.leftTurn = false;
	this.rightTurn = false;
	this.min_hue = 0;
	this.max_hue = 360;
}
var dt = 0.3;
var dphi = 0.6;

player.prototype.step = function() {
	if(this.leftTurn) {
		this.direction -= time*Math.PI*dphi;
	}
	if(this.rightTurn) {
		this.direction += time*Math.PI*dphi;
	}
	this.position.x += time*Math.sin(this.direction)*dt;
	this.position.y += time*Math.cos(this.direction)*dt;
	this.temp_trace.push(new point(this.position.x, this.position.y));
}

player.prototype.addPoint = function(pnt, direction)
{
	this.position = pnt;
	this.direction = direction;
	this.server_trace.push(pnt);
	this.temp_trace.length = 0;
}

function WebGLPrepare(users) {
	players = new Array();
  	$.each(users, function(id, user){
    		players[user.nick] = new player(new point(user.x, user.y), user.direction);
  	});
	var plrcnt = 0;
	for(xplr in players){
		plrcnt++;
	}
	if(plrcnt < 4){
		var ah=0;
		for(xplr in players) {
			players[xplr].min_hue = ah;
			players[xplr].max_hue = ah + 180/plrcnt;
			ah += 360/plrcnt;
			ui_set_user_color(xplr, rgbtostring(hsv2rgb(new hsv(players[xplr].min_hue, 1, 1))), 
					rgbtostring(hsv2rgb(new hsv(players[xplr].max_hue, 1, 1))))
		}
	} else {
		var ah=0;
		for(xplr in players) {
			players[xplr].min_hue = ah;
			players[xplr].max_hue = ah;
			ui_set_user_color(xplr, rgbtostring(hsv2rgb(new hsv(players[xplr].min_hue, 1, 1))), 
					rgbtostring(hsv2rgb(new hsv(players[xplr].max_hue, 1, 1))))
			ah += 360/plrcnt;
		}
	}

	init();
}

function WebGLStart() {
	animation = true;
}
