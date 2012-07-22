var current_screen = 'welcome';
function ui_show_screen(name){
    $("#screen_"+current_screen).hide();
    $("#screen_"+name).show();
    current_screen = name;
}
function ui_set_status(stat){
    $(".ui_status").html(stat);
}

$(function(){
    $.templates({
        room_entry: "#room_entry_template",
    });
});

function ui_update_room_list(rooms){
    $( "#room_table tbody" ).html(
        $.render.room_entry( rooms )
    );
}
function ui_update_room(people){
    $("#screen_room_id").html(config.current_room);
    $("#screen_room_members").html();
    $.each(people, function(id,nick){
      $("#screen_room_members").append('<li>'+nick+'</li>');
    });
    $("#screen_room_btn_start").hide();
    if(config.is_room_owner){
      $("#screen_room_btn_start").show();
    }
}
function ui_set_failure(stat){
    $("#screen_error_status").html(stat);
}

$(function() {
    $("#hello").click(login);
    $("#screen_room_btn_start").click(start_game);
    $("#create_room_btn").click(function(){
        //var name = $("#create_room_input").val();
        create_room();
        //var name = $("#create_room_input").val("");
    });
});
