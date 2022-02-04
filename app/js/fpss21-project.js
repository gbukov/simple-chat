/**
  Add new message to the main window (chat)
  msg   - the message, example:
          "@message:XXX;@name:YYY;@room:ZZZ;@date:2021-06-24 19:53:11"
*/
function printMsg(msg) {
  marr = msg.split(';');
  switch (marr[0].split(':')[0]) {
    case '@message':
    case '@private':
      // split message to vars
      date = marr[3].slice(17, 22);
      name = marr[1].split(':')[1];
      room = marr[2].slice(6);
      mssg = marr[0].slice(9);
      // get type of message (normal or private)
      type = marr[0].split(':')[0].slice(1) == 'private' ? 'private' : 'normal';
      // if it's user's message -> right, otherwise -> left
      if (uname == name) {
        align = `class="msg-general right ${type}"`;
        if (type == 'private') name = '<span>private from:</span> ' + name;
        flex  = 'class="flex-right"';
      } else {
        if (type == 'private') name = '<span>private from:</span> ' + name;
        align = `class="msg-general left ${type}"`;
        flex  = '';
      }
      $('#repl').append(`
        <li ${flex}><div ${align}>
          <div>
            <span id="msg-box-name">${name}</span>
            <span id="msg-box-room">(${room})</span>:
            <span id="msg-box-date">${date}</span>
          </div>
          <div>${mssg}</div>
          </div>
        </li>`);
      break;
    case '@system':
      $('#repl').append(`
        <li><div class='smsg'>
            <span>${marr[0].slice(8)} ${marr[1].slice(9)}</span>
        </div></li>`);
      break;
    default:

  }
}

/**
  Remove all children in a list (lname) and add new + add function onclick()
  lname - list's id (<li id='xxx'>)
  msg   - the message, example "@listOfX:aa;bb;cc;"
*/
function updateListOf(lname, msg, cls) {
  $(`#${lname}`).children().remove();
  $.each(msg.split(':')[1].split(';').slice(0, -1), function(i, v) {
    $(`#${lname}`).append(`<li><div>${v}</div></li>`);
  });
  switch (msg.split(':')[0]) {
    case '@listOfRooms':
      $(`#${lname} > li`).click(function() {
        room = $(this).first().text();
        $('#repl').children().remove();
        connectionToServer.send('@room ' + room);
        prompt.value = "";
      });
      break;
    case '@listOfUsersInRoom':
      $(`#${lname} > li`).click(function() {
        name = $(this).first().text();
        prompt.value = `@private ${name} `;
      });
      break;
    default:
      alert('Error: updateListOf(): lname not a list\'s id');
  }
}


// -- -- -- -- -- --  Init function  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
$(document).ready(function() {
  // global user name
  window.uname = "";

  // disable all fields at start
  $('#prompt').attr('disabled', 'disabled');
  $('#promptName').attr('disabled', 'disabled');
  $('#enterName button').attr('disabled', 'disabled');

  // First of all we need a socket:
  window.prompt = document.getElementById('prompt');
  window.connectionToServer = new WebSocket("ws://localhost:8080/");

  connectionToServer.onopen = function(e) {
    $('#promptName').removeAttr('disabled');
    $('#enterName button').removeAttr('disabled');
    prompt.onkeydown = function (e) {
      if (e.keyCode == 13) {
        if (prompt.value.split(" ")[0] == "@room") {
          $('#repl').children().remove();
        }
        connectionToServer.send(prompt.value);
        prompt.value = "";
      }
    }
  };
  connectionToServer.onmessage = function (m) {
    switch (m.data.split(":")[0]) {
      case "@system":
      case "@message":
      case "@private":
        printMsg(m.data);
        break;
      case "@introducing":
        $('#enterName div:first-child').text(m.data.split(':')[1]);
        break;
      case "@introducing-succ":
        $('#enterName').css('display', 'none');
        $('.container').css('filter', 'blur(0px)');
        $('#prompt').removeAttr('disabled');
        window.uname = $('#promptName').val();
        break;
      case "@listOfRooms":
        updateListOf("rooms", m.data, "");
        break;
      case "@listOfUsersInRoom":
        updateListOf("users", m.data, "");
        break;
      default:
        alert("Find me! (c)bug");
    }
  }

  // Prepare onclick function for welcome window
  $('#promptName').val("");
  $('#enterName button').click(function() {
    connectionToServer.send('@name ' + $('#promptName').val())
  });

  // Prepare 'new room' button
  $('#addRoom').css('display', 'none');
  $('#newRoomBtn').click(function() {
    $('#promptAddRoom').val('');
    $('#addRoom').css('display', '');
    $('.container').css('filter', 'blur(2px)');
  });
  $('#addRoom button').click(function() {
    connectionToServer.send('@room ' + $('#promptAddRoom').val());
    $('#repl').children().remove();
    $('#addRoom').css('display', 'none');
    $('.container').css('filter', 'blur(0px)');
  });

  // Two Themes for main wendow
  $('#light').css('text-decoration', 'underline');
  $('body').css('background-image', 'url("css/bg_1.png")');
  $('#light').click(function() {
    $('#dark').css('text-decoration', '');
    $('#light').css('text-decoration', 'underline');
    $('body').css('background-image', 'url("css/bg_1.png")');
  });
  $('#dark').click(function() {
    $('#light').css('text-decoration', '');
    $('#dark').css('text-decoration', 'underline');
    $('body').css('background-image', 'url("css/bg_2.jpg")');
  });
});
