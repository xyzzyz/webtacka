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
        score_board: "#score_board_template",
        error: "#error_template",
    });
});

function ui_update_room_list(rooms){
    $.each(rooms, function(id, room){
      room.people = JSON.stringify(room.people);
    });
    $( "#room_table tbody" ).html(
        $.render.room_entry( rooms )
    );
    $(".screen_room_list_join_btn").click(function(){
        var room = parseInt($(this).attr('room_id'));
        join_room(room);
    });
}
function ui_update_room(people){
    $("#screen_room_id").html(config.current_room);
    $("#screen_room_members").html("");
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

function ui_update_score_board_colors(people_colors){
}

function ui_set_user_color(nick, color1, color2){
  config.users[nick].color1 = color1;
  config.users[nick].color2 = color2;
}

function ui_render_score_board(people){
    $( "#screen_room_score_board" ).html(
        $.render.score_board(people)
    );
    $(".score_board_entry").each(function(index){
      var nick = $(this).attr('nick');
      var color1 = config.users[nick].color1;
      var color2 = config.users[nick].color2;
      $(this).find(".user_color").css('background-image', 'linear-gradient(bottom, ' + color1 + ' 27%, ' + color2 + ' 64%)');
      $(this).find(".user_color").css('background-image', '-o-linear-gradient(bottom, ' + color1 + ' 27%, ' + color2 + ' 64%)');
      $(this).find(".user_color").css('background-image', '-moz-linear-gradient(bottom, ' + color1 + ' 27%, ' + color2 + ' 64%)');
      $(this).find(".user_color").css('background-image', '-webkit-linear-gradient(bottom, ' + color1 + ' 27%, ' + color2 + ' 64%)');
      $(this).find(".user_color").css('background-image', '-ms-linear-gradient(bottom, ' + color1 + ' 27%, ' + color2 + ' 64%)');
      $(this).find(".user_color").css('background-image', '-webkit-gradient( linear, left bottom, left top, color-stop(0.27, ' + color1 + '), color-stop(0.64, ' + color2 + '));');
  });

};

function ui_show_error(err) {
    $( "#errors" ).append(
    $.render.error( [{'error': err}]));
};

function ui_show_end_game() {
  $("#lesson01-canvas").hide();
  $("#screen_game_play_again_btn").hide();
  $("#screen_game_end").show();
  if(config.is_room_owner){
    $("#screen_game_play_again_btn").show();
  }
}

function ui_show_about() {
  $('#about_modal').modal();
};


$(function() {
    $("#hello").click(login);
    $("#screen_room_btn_start").click(start_game);
    $("#screen_game_play_again_btn").click(continue_game);
    $("#create_room_btn").click(function(){
        var capacity = parseInt($("#create_room_capacity").val());
        create_room(capacity);
        $("#create_room_capacity").val("");
    });

    $("#room_list_refresh_btn").click(function(){
      refresh_room_list();
    });
    $("#about_btn").click(ui_show_about);


    $('#nick').keypress(function(e){
      if(e.which == 13){
       $('#hello').click();
       }
    });
    $('#create_room_capacity').keypress(function(e){
      if(e.which == 13){
       $('#create_room_btn').click();
       }
    });
});
