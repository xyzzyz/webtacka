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
}
function update_room_list(rooms){
    ui_update_room_list(rooms);
    ui_show_screen("room_list");
};

function create_room(){
    ui_set_status("Creating room...");
    ui_show_screen("wait");
    send_create_room();
}

function joined_to_room(id, owner){
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

function fail(err){
    ui_set_failure(err);
    ui_show_screen("error");
};
