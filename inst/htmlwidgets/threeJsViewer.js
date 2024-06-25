HTMLWidgets.widget({
  name: "threeJsViewer",
  type: "output",
  
  factory: function(el, width, height){
    // import tjviewer class
    const importPromise = import('../tjviewer-0.0.1/tjviewer.module.js');
    async function run() {
      const {tjViewer} = await importPromise;
      var obj = new tjViewer(el, width, height);
      return(obj);
    }
    
    
    var w = parseInt(width);
    var h = parseInt(height);
    if(w == 0) w = 1;
    if(h == 0) h = 1;
    
    return {
      obj: run,
      
      renderValue: function(x) {
        run().then(
          function(value) {
            value.create_plot(x);
            value.animate();
          },
          function(error) {
            console.log(error);
          }
        );
      },
      
      resize: function(width, height){
        run().then(
          function(value) {
            value.onWindowResize(width, height);
            value.animate();
          },
          function(error) {
            console.log(error);
          }
        );
      }
    };
  }
})

