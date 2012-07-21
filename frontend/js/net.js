var ws;
$(function(){
    console.log('connecting to '+ config.server_url);
    ws = $.websocket(config.server_url,
            {
                open: function () {
                    console.log('open');
                },
                close: function() {
                    console.log('closed');
                },
                events: {
                    'OK':function(e){
                        console.log("received OK message :)");
                    },
                    'logged_in': function(e){
                        logged_in();
                    },
                    'login_err': function(e){
                        login_err();
                    },
                    'room_list': function(e){
                        update_room_list(e.data);
                    },
                    'joined': function(e){
                        joined(e.data.id);
                    },
                    'room_data': function(e){
                        update_room(e.data.people);
                    },
                    'err': function(e){
                        fail("");
                    },
                }
            });
});

function lsend(type, params){
    console.log('Sending ' + type +' message - data: '+JSON.stringify(params));
    ws.send(type, params);
}

function send_hello(nick){
    lsend('hello', {
        'nick': nick,
    });
}

function send_get_rooms(){
    lsend('get_rooms');
}
function send_create_room(){
    lsend('create_room');
}
function send_start(){
    lsend('start');
}

function send_join(room_id){
    lsend('join', { 'room': room_id});
}

