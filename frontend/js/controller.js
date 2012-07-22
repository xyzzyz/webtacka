function log(x){
    console.log(JSON.stringify(x));
};

$(function() {
    ui_show_screen('welcome');
});


function login(){
    send_hello($("#nick").val());
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


function prepare_game(users){
    if(typeof(config.users) == "undefined"){
      config.users = users;
      $.each(config.users, function(id, user){
        user.score = 0;
        user.color1 = "#FFBD24";
        user.color2 = "#FFF700";
      });
    }
    ui_render_score_board(config.users);
    ui_show_screen("game");
    WebGLStart();
};

//function start_game(){
//}

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
