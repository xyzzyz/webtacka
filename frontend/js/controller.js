function log(x){
    console.log(JSON.stringify(x));
};

$(function() {
    ui_show_screen('welcome');
});


function login(){
    config.nick = $("#nick").val();
    send_hello(config.nick);
    ui_set_status("Logging in.");
    ui_show_screen("wait");
};


function logged_in() {
    send_get_rooms();
    ui_set_status("Fetching room list...");
    ui_show_screen("wait");
}

function refresh_room_list() {
    send_get_rooms();
    ui_set_status("Fetching room list...");
}



function update_room_list(rooms){
    ui_update_room_list(rooms);
    log(rooms);
    ui_show_screen("room_list");
};

function create_room(capacity){
    ui_set_status("Creating room...");
    ui_show_screen("wait");
    send_create_room(capacity);
}

function join_room(id){
    ui_set_status("Joining to room #"+id+"...");
    ui_show_screen("wait");
    send_join(id);
}
function joined(id, owner){
    ui_set_status("Joined to room #"+id+"... Fetching room data...");
    config.current_room = id;
    config.is_room_owner = owner;
    ui_show_screen("wait");
}

function update_room(people){
    ui_update_room(people);
    ui_show_screen("room");
};

function start_game(){
    ui_set_status("Starting game...");
    ui_show_screen("wait");
    send_start();
};

function control_turn(direction){
  send_turn(direction);
}


function prepare_game(users){
    if(typeof(config.users) == "undefined"){
      var newusers = {};
      $.each(users, function(id, user){
        newusers[user.nick] = {
          nick: user.nick,
          x: user.x,
          y: user.y,
          direction: user.direction,
          score: 0,
          color1: "#FFFFFF",
          color2: "#000000"
        };
        log(newusers[user.nick]);
        users[id] = newusers[user.nick];
        log(newusers);
      });
      config.users = newusers;
      config.users_list = users;
    }
    ui_render_score_board(config.users_list);
    ui_show_screen("game");
    WebGLPrepare(config.users);
    ui_render_score_board(config.users_list);
};

function update_scoreboard(board){
  $.each(board, function(id, e){
    config.users[e.nick].score =  e.score;
  });
  ui_render_score_board(config.users_list);

}

function game_started(){
    WebGLStart();
}

function game_tick(moves){
  $.each(moves, function(id, move){
    pointFromServer(move.nick, new point(move.x, move.y), move.direction);
  });
}

function fail(err){
    ui_set_failure(err);
    ui_show_screen("error");
};

function login_err(err){
    ui_show_error(err);
    ui_show_screen("welcome");
};
function join_err(err){
    ui_show_error(err);
    logged_in();
};
function player_dead(nick){
  kill(nick);
}
function end_game(nick){
  ui_show_end_game();
}
