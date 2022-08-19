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
    }).then(\res -> res.json());

  var inp = document.querySelector '#expr';
  var form = document.querySelector '#calc';
  var box = document.querySelector '#box';
  var text = document.querySelector '#text';
  var subtext = document.querySelector '#subtext';

  fun process {| ok:ok, output:output, annotation:annotation |} {
    console.log "in process";
    box.classList.remove 'success' 'failure';
    if (!ok) {
      text.innerHTML = '✖ Error\n' + output;
      box.classList.add 'failure';
      subtext.innerHTML = '';
      return;
    } else {
      text.innerHTML = output;
      box.classList.add 'success';
      subtext.innerHTML = annotation;
    }
  }

  form.addEventListener('submit', \e {
    e.preventDefault();
    evaluate(inp.value.trim()).then \r -> process r;
  });
|]
