{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Site.JS where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Language.Javascript.JMacro

-- lol
frontpageJS :: JStat
frontpageJS = [jmacro|
  fun evaluate expr ->
    fetch('/calc', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify { input: expr }
    }).then \res -> res.json();

  var inp = document.querySelector '#expr';
  var form = document.querySelector '#calc';
  var box = document.querySelector '#box';

  fun process {| ok:ok, output:output, sigfigs:sigfigs |} {
    console.log "in process";
    if (!ok) {
      box.innerHTML = output;
      return;
    } else {
      box.innerHTML = output + ' (' + sigfigs + ')';
    }
  }

  form.addEventListener('submit', \e {
    e.preventDefault();
    evaluate(inp.value.trim()).then \r -> process r;
  });
|]
