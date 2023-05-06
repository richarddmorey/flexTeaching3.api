//<![CDATA[
// https://stackoverflow.com/a/21814964/1129889
document.onreadystatechange = function () {
    if (document.readyState == "interactive") {
        // https://stackoverflow.com/a/41896059/1129889
        document.querySelectorAll('a')
        .forEach((el)=>{ 
            if ( !(el.pathname == window.location.pathname &&
                   el.protocol == window.location.protocol &&
                   el.host == window.location.host &&
                   el.hash) ){
                    // https://stackoverflow.com/a/60186506/1129889
                    el.target = '_blank';
                    // https://cheatsheetseries.owasp.org/cheatsheets/HTML5_Security_Cheat_Sheet.html#tabnabbing
                    el.rel = 'noopener noreferrer';
            }
        });
    }
}

//]]>
