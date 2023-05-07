//<![CDATA[
// https://stackoverflow.com/a/27564637/1129889
// This is to be injected into all HTML files that are not fragments.
function sendHeight()
{
    if(parent.postMessage)
    {
        const height = document.documentElement.scrollHeight;
        parent.postMessage( JSON.stringify({ "height": height }), '*');
    }
}

window.addEventListener("DOMContentLoaded", (event) => {
  // Do this 5 times, once every 500ms, just to make sure we 
  // account for slow scripts (eg MathJax)
  for (let i = 0; i < 5; i++) {
    setTimeout( sendHeight, 500 * i);
  }
  //setInterval(sendHeight, 500);
});

//]]>
