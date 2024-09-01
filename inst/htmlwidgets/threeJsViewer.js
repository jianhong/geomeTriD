HTMLWidgets.widget({
  name: "threeJsViewer",
  type: "output",
  
  factory: function(el, width, height){
    var canvas;
    // import tjviewer class
    const importPromise = import('https://cdn.jsdelivr.net/gh/jianhong/geometrid/inst/htmlwidgets/lib/tjviewer/tjviewer.module.js');
    async function run() {
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
                  canvas = new tjViewer(el, width, height);
                  canvas.create_plot(x);
                  canvas.animate();
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

