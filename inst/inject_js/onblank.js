//<![CDATA[
// https://stackoverflow.com/a/21814964/1129889
document.onreadystatechange = function () {
    if (document.readyState == "interactive") {
        // https://stackoverflow.com/a/41896059/1129889
        document.querySelectorAll('a')
        .forEach((el)=>{ 
            if ( el.pathname == window.location.pathname &&
                 el.protocol == window.location.protocol &&
                 el.host == window.location.host &&
                 el.hash ){
              // https://matthewmanela.com/blog/making-linking-between-anchors-in-an-iframe-work-in-firefox-11-and-above/
             $(el).click(()=>{
                const nameElement = $("[name='" + el.hash.substring(1) + "']");
                const idElement = $(el.hash);
                var element = null;
                if (nameElement.length > 0) {
                  element = nameElement;
                } else if (idElement.length > 0) {
                  element = idElement;
                }
 
                if (element) {
                  const offset = element.offset();
                  parent.postMessage( JSON.stringify({ scroll: offset }), '*' );
                }
 
                return false;
              });
                   
            } else {
              // https://stackoverflow.com/a/60186506/1129889
              el.target = '_blank';
              // https://cheatsheetseries.owasp.org/cheatsheets/HTML5_Security_Cheat_Sheet.html#tabnabbing
              el.rel = 'noopener noreferrer';
            }
        });
    }
}

//]]>
