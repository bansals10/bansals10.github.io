// document.addEventListener('DOMContentLoaded', function() {
//   window.onscroll = function() {myFunction()};
//
//   var navbar = document.getElementById("main-nav");
//   var sticky = navbar.offsetTop;
//
//   function myFunction() {
//     if (window.pageYOffset >= sticky) {
//       navbar.classList.add("sticky")
//     } else {
//       navbar.classList.remove("sticky");
//     }
//   }
// })

var withBreaks = "Hello World. \n My name is Jennifer. \n What is your name?"


$(document).ready(function() {
  $('#main-nav li a').click(function(e) {

    var targetHref = $(this).attr('href');

    $('html, body').animate({
      scrollTop: $(targetHref).offset().top
    }, 1000);

    e.preventDefault();
  });
});

document.addEventListener('DOMContentLoaded', function() {
  var i = 0;

  var txt = "software developer, sophomore at UChicago, and advocate for women in tech";
  var speed = 50;

  function typeWriter() {
    if (i < txt.length) {
      document.getElementById("demo").innerHTML += txt.charAt(i);
      i++;
      setTimeout(typeWriter, speed);
    }
  }

  // typeWriter()
})
