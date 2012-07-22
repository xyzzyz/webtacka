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
                        login_err(e.data.error);
                    },
                    'join_err': function(e){
                        join_err(e.data.error);
                    },
                    'room_list': function(e){
                        log(e);
                        update_room_list(e.data.rooms);
                    },
                    'joined': function(e){
                        log(e);
                        joined(e.data.id, e.data.owner);
                    },
                    'room_data': function(e){
                        log(e);
                        update_room(e.data.people);
                    },
                    'protocol_error': function(e){
                        fail(e.data.error);
                    },
                    'prepare': function(e){
                      prepare_game(e.data.people);
                    },
                    'game_started': function(e){
                      game_started();
                    },
                    'game_tick': function(e){
                      game_tick(e.data.moves);
                    },
                    'player_dead': function(e){
                      player_dead(e.data.nick);
                      update_scoreboard(e.data.scoreboard);
                    },
                    'game_ended': function(e){
                      end_game();
                    },
                }
            });
});

function lsend(type, params){
    if(typeof(params) == "undefined"){
      params = {};
    }

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
function send_create_room(capacity){
    lsend('create_room', {'capacity':capacity});
}
function send_start(){
    lsend('start');
}

function send_continue(){
    lsend('continue');
}

function send_join(room_id){
    lsend('join', { 'room': room_id});
}
function send_turn(direction){
    lsend('client_move', { 'direction':direction});
}

