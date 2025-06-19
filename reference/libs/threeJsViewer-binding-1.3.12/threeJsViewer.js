HTMLWidgets.widget({
  name: "threeJsViewer",
  type: "output",
  
  factory: function(el, width, height){
    var canvas;
    
    async function run(url= '../tjviewer-0.0.1/tjviewer.module.js') {
        // import tjviewer class
        const importPromise = import(url);
        const {tjViewer} = await importPromise;
        var obj = new tjViewer(el, width, height);
        return(obj);
    }
     
    return {
        canvas: canvas,
        
        renderValue: function(x) {
          run().then(
            function(value){
                  canvas = value;
                  canvas.create_plot(x);
                  canvas.animate();
            },
            function(error){
                  console.log(error);
                  run(url = 'https://cdn.jsdelivr.net/gh/jianhong/geometrid/inst/htmlwidgets/lib/tjviewer/tjviewer.module.js').then(
                      function(value){
                            canvas = value;
                            canvas.create_plot(x);
                            canvas.animate();
                      },function(error){
                        console.log(error);
                      }
                  );
            }
          );
        },
        
        resize: function(width, height){
              canvas.onWindowResize(width, height);
              canvas.animate();
        }
      };
  }
})

