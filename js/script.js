document.addEventListener('DOMContentLoaded', function() {
  window.onscroll = function() {myFunction()};

  var header = document.getElementById("logo");
  var sticky = header.offsetTop;

  function myFunction() {
    if (window.pageYOffset > sticky) {
      header.classList.add("sticky");
    } else {
      header.classList.remove("sticky");
    }
  }
})
