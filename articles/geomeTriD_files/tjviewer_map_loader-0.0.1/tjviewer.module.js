// please note that the relative folder is related with the version number.
import * as THREE from 'three';
//import Stats from 'three/addons/libs/stats.module.js';
//import { GPUStatsPanel } from 'three/addons/utils/GPUStatsPanel.js';
import { GUI } from 'three/addons/libs/lil-gui.module.min.js';
import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
// lines
import { Line2 } from 'three/addons/lines/Line2.js';
import { LineMaterial } from 'three/addons/lines/LineMaterial.js';
import { LineGeometry } from 'three/addons/lines/LineGeometry.js';
import { LineSegments2 } from 'three/addons/lines/LineSegments2.js';
import { LineSegmentsGeometry } from 'three/addons/lines/LineSegmentsGeometry.js';
// text
import { Font } from 'three/addons/loaders/FontLoader.js';
import { TextGeometry } from 'three/addons/geometries/TextGeometry.js';
// label , local patch 
import { CSS2DRenderer, CSS2DObject } from 'tjviewer/CSS2DRenderer.js';
// exporters
import { SVGRenderer } from 'three/addons/renderers/SVGRenderer.js';
import { GLTFExporter } from 'three/addons/exporters/GLTFExporter.js';
import { PLYExporter } from 'three/addons/exporters/PLYExporter.js';
import { STLExporter } from 'three/addons/exporters/STLExporter.js';
import { DRACOExporter } from 'three/addons/exporters/DRACOExporter.js';
// module Widget
class tjViewer{
  constructor(el, width, height){
    // clear el
    el.textContent = '';
    //add gui first
    this.gui = new GUI({container:el});
    function dragGUI(elmnt){
      var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
      elmnt.children[0].onmousedown = dragMouseDown;
      function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        // get the mouse cursor position at startup:
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        // call a function whenever the cursor moves:
        document.onmousemove = elementDrag;
      }
      function elementDrag(e) {
        e = e || window.event;
        e.preventDefault();
        // calculate the new cursor position:
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // set the element's new position:
        elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
      }
      function closeDragElement() {
        // stop moving when mouse button is released:
        document.onmouseup = null;
        document.onmousemove = null;
      }
    }
    dragGUI(this.gui.domElement);
    
    //viewer
    this.width = width;
    this.height = height;
    // viewport
    this.insetWidth = height / 4; // square
    this.insetHeight = height / 4;
    
    this.renderer = new THREE.WebGLRenderer( { antialias: true } );
    this.renderer.setPixelRatio( window.devicePixelRatio );
    this.renderer.setSize( width, height );
    this.renderer.setClearColor( 0x000000, 0.0 );
    this.animate = this.animate.bind(this);
    this.renderer.setAnimationLoop( this.animate );
    el.appendChild(this.renderer.domElement);
    
    // label renderer
    this.labelRenderer = new CSS2DRenderer();
    this.labelRenderer.setSize( width, height );
    this.labelRenderer.domElement.style.position = 'relative';
    this.labelRenderer.domElement.style.top = '-'+this.height+'px';
    this.labelRenderer.domElement.style.left = '0px';
    el.appendChild(this.labelRenderer.domElement);
    this.labelRenderer2 = new CSS2DRenderer();
    this.labelRenderer2.setSize( width/2, height );
    this.labelRenderer2.domElement.style.position = 'relative';
    this.labelRenderer2.domElement.style.top = '-'+2*this.height+'px';
    this.labelRenderer2.domElement.style.left = this.width/2+'px';
    
    this.scene = new THREE.Scene();
    this.sceneBottom = new THREE.Scene();
    this.scene2 = new THREE.Scene();
    this.sceneBottom2 = new THREE.Scene();
    this.overlay = false;
    this.sideBySide = false;
    
    this.materials = [];
    this.objects = new THREE.Group();
    this.objectsBottom = new THREE.Group();
    this.background = new THREE.Color(1, 1, 1);
    this.bckalpha = 1;
    this.backgroundBottom = new THREE.Color(1, 1, 1);
    this.bckalphaBottom = 1;
    this.objects2 = new THREE.Group();
    this.objectsBottom2 = new THREE.Group();
    this.background2 = new THREE.Color(1, 1, 1);
    this.bckalpha2 = 1;
    this.backgroundBottom2 = new THREE.Color(1, 1, 1);
    this.bckalphaBottom2 = 1;
    
    this.slider = document.createElement('div');
    this.slider.className = 'tjviewer_slider';
    this.slider.style.display = 'none';
    this.container = el;
    el.appendChild(this.slider);
    this.sliderPos = this.height/2;
    
    this.resizeBlock = document.createElement('div');
    this.resizeBlock.className = 'tjviewer_resizeblock';
    this.setResizeBlockPos();
    el.appendChild(this.resizeBlock);
    this.resizeCanvas(this.resizeBlock);
      
    var near = .0001
    var far = 100
    var fov = 50
    this.camera = new THREE.PerspectiveCamera( fov, width / height, near, far );
    this.camera.position.set( 0, 0, -far/10 ); // set to x, y, z;
    this.camera.layers.enableAll();
    this.camera2 = new THREE.PerspectiveCamera( fov, width / height, near, far );;
    this.camera2.position.set( 0, 0, -far/10 ); // set to x, y, z;
    this.camera2.layers.enableAll();
    
    // viewport
    this.cameraInsert = new THREE.PerspectiveCamera( fov, 1, near, far );
    this.cameraInsert.position.copy( this.camera.position );
    this.cameraInsert.layers.enableAll();
    
    // mouse controls
    this.controls = new OrbitControls( this.camera, this.labelRenderer.domElement );
    this.controls.enableDamping = true;
    this.controls.minDistance = near*2;
    this.controls.maxDistance = far/2;
    
    this.controls2 = new OrbitControls( this.camera2, this.labelRenderer2.domElement );
    this.controls2.enableDamping = true;
    this.controls2.minDistance = near*2;
    this.controls2.maxDistance = far/2;
    
    /*
    this.stats = new Stats();
    el.appendChild(this.stats.dom);
    
    this.gpuPanel = new GPUStatsPanel( this.renderer.getContext() );
    this.stats.addPanel( this.gpuPanel );
    this.stats.showPanel( 0 );*/
    
    el.parentElement.addEventListener('wheel', (event)=>{
      // Infinity zoom in.
      //this.camera.fov += event.deltaY*0.005;
      if(this.camera.fov<=0.1) this.camera.fov=0.1;
      //console.log(this.camera.fov);
      this.camera.updateProjectionMatrix();
      if(this.sideBySide){
        //this.camera2.fov += event.deltaY*0.005;
        if(this.camera2.fov<=0.1) this.camera2.fov=0.1;
        this.camera2.updateProjectionMatrix(); 
      }
    })
    
    // camera ratio GUI
    const cameraparam = {
      YX_aspect : this.camera.aspect,
      world_center : function(){
        this.objects.position.set(0, 0, 0);
        if(this.overlay){
          this.objectsBottom.position.set(0, 0, 0);
        }
        if(this.sideBySide){
          this.objects2.position.set(0, 0, 0);
          if(this.overlay){
            this.objectsBottom2.position.set(0, 0, 0);
          }
        }
      }.bind(this),
      object_center : function(){
        var bbox = new THREE.Box3().setFromObject(this.objects);
        var x = -(bbox.min.x+bbox.max.x)/2;
        var y = -(bbox.min.y+bbox.max.y)/2;
        var z = -(bbox.min.z+bbox.max.z)/2;
        this.objects.translateX(x);
        this.objects.translateY(y);
        this.objects.translateZ(z);
        if(this.overlay){
          this.objectsBottom.translateX(x);
          this.objectsBottom.translateY(y);
          this.objectsBottom.translateZ(z);
        }
        if(this.sideBySide){
          bbox = new THREE.Box3().setFromObject(this.objects2);
          x = -(bbox.min.x+bbox.max.x)/2;
          y = -(bbox.min.y+bbox.max.y)/2;
          z = -(bbox.min.z+bbox.max.z)/2;
          this.objects2.translateX(x);
          this.objects2.translateY(y);
          this.objects2.translateZ(z);
          if(this.overlay){
            this.objectsBottom2.translateX(x);
            this.objectsBottom2.translateY(y);
            this.objectsBottom2.translateZ(z);
          }
        }
      }.bind(this),
      center_x : 0,
      center_y : 0,
      center_z : 0,
      changed : false,
      setAspect : function(){
        this.camera.aspect = cameraparam.YX_aspect;
        this.camera2.aspect = cameraparam.YX_aspect;
        this.camera.updateProjectionMatrix();
        this.camera2.updateProjectionMatrix();
      }.bind(this),
      setPosition : function(){
        this.objects.position.set(
          cameraparam.center_x,
          cameraparam.center_y,
          cameraparam.center_z);
        if(this.overlay){
          this.objectsBottom.position.set(
            cameraparam.center_x,
            cameraparam.center_y,
            cameraparam.center_z);
        }
        if(this.sideBySide){
          this.objects2.position.set(
            cameraparam.center_x,
            cameraparam.center_y,
            cameraparam.center_z);
          if(this.overlay){
            this.objectsBottom2.position.set(
              cameraparam.center_x,
              cameraparam.center_y,
              cameraparam.center_z);
          }
        }
      }.bind(this)
    }
    const cameraGUI = this.gui.addFolder('camera');
    cameraGUI.add(cameraparam, 'YX_aspect', 0, 10).onChange( function(val){
      cameraparam.YX_aspect = val;
      cameraparam.changed = true;
    }).onFinishChange(cameraparam.setAspect);
    cameraGUI.add(cameraparam, 'center_x', -10, 10).onChange( function(val){
      cameraparam.center_x = val;
      cameraparam.setPosition();
    });
    cameraGUI.add(cameraparam, 'center_y', -10, 10).onChange( function(val){
      cameraparam.center_y = val;
      cameraparam.setPosition();
    });
    cameraGUI.add(cameraparam, 'center_z', -10, 10).onChange( function(val){
      cameraparam.center_z = val;
      cameraparam.setPosition;
    });
    cameraGUI.add(cameraparam, 'world_center');
    cameraGUI.add(cameraparam, 'object_center');
    cameraGUI.close();
    
    
    this.maxRadius = 1;
    this.maxLineWidth = 50;
    // search GUI
    const searchGUI = this.gui.addFolder('search');
    const searchparam = {
      keyword : '',
      search : function(){
        let result = [];
        var isCoor = searchparam.keyword.match(/^([^a-zA-Z]+[kKmMgG])(\-[^a-zA-Z]+[kKmMgG])*$/g);
        if(isCoor){
          var keyword = searchparam.keyword.replace(',', '')
                                   .split(/[^0-9.kKmMgG]+/);
          if(keyword.length>2){
            alert('More than two number detected. Coordinates should be in the format of 12345-45678.');
          }else{
            for(var i=0; i<keyword.length; i++){
              var ii = parseFloat(keyword[i]);
              if(keyword[i].match(/[kK]$/)){
                ii = ii * 1000
              }
              if(keyword[i].match(/[mM]$/)){
                ii = ii * 1000000
              }
              if(keyword[i].match(/[gG]$/)){
                ii = ii * 1000000000
              }
              keyword[i] = ii
            }
            let pos = [];
            this.scene.traverse(obj => {
              if(obj.isLine2 === true){
                if(typeof obj.geometry.userData.start != "undefined" &&
                   typeof obj.geometry.userData.end != "undefined"){
                  const start = obj.geometry.userData.start;
                  const end = obj.geometry.userData.end;
                  let seg = [];
                  if(keyword.length==1){
                    for(var i=0; i<start.length; i++){
                      if(start[i]<=keyword[0] && end[i] >= keyword[0]){
                        seg.push(i);
                      }
                    }
                  }else{
                    for(var i=0; i<start.length; i++){
                      if(start[i]>=keyword[0] && end[i] <= keyword[1]){
                        seg.push(i);
                      }
                    }
                  }
                  if(seg.length>0){
                    const instanceStart = obj.geometry.attributes.instanceStart.data.array;
                    for(var i=0; i<seg.length; i++){
                      pos.push(instanceStart[seg[i]*6]);
                      pos.push(instanceStart[seg[i]*6+1]);
                      pos.push(instanceStart[seg[i]*6+2]);
                      pos.push(instanceStart[seg[i]*6+3]);
                      pos.push(instanceStart[seg[i]*6+4]);
                      pos.push(instanceStart[seg[i]*6+5]);
                    }
                  }
                }
              }
            });
            this.sceneBottom.traverse(obj => {
              if(obj.isLine2 === true){
                if(typeof obj.geometry.userData.start != "undefined" &&
                   typeof obj.geometry.userData.end != "undefined"){
                  const start = obj.geometry.userData.start;
                  const end = obj.geometry.userData.end;
                  let seg = [];
                  if(keyword.length==1){
                    for(var i=0; i<start.length; i++){
                      if(start[i]<=keyword[0] && end[i] >= keyword[0]){
                        seg.push(i);
                      }
                    }
                  }else{
                    for(var i=0; i<start.length; i++){
                      if(start[i]>=keyword[0] && end[i] <= keyword[1]){
                        seg.push(i);
                      }
                    }
                  }
                  if(seg.length>0){
                    const instanceStart = obj.geometry.attributes.instanceStart.data.array;
                    for(var i=0; i<seg.length; i++){
                      pos.push(instanceStart[seg[i]*6]);
                      pos.push(instanceStart[seg[i]*6+1]);
                      pos.push(instanceStart[seg[i]*6+2]);
                      pos.push(instanceStart[seg[i]*6+3]);
                      pos.push(instanceStart[seg[i]*6+4]);
                      pos.push(instanceStart[seg[i]*6+5]);
                    }
                  }
                }
              }
            });
            if(pos.length>=6){
                    const geometry = new LineSegmentsGeometry();
                    geometry.setPositions( pos );
                    const box = geometry.boundingBox;
                    this.camera.position.set(
                      (box.max.x+box.min.x),
                      (box.max.y+box.min.y),
                      (box.max.z+box.min.z) );
                    this.animate();
            }
            if(this.sideBySide){
                pos = [];
                this.scene2.traverse(obj => {
                  if(obj.isLine2 === true){
                    if(typeof obj.geometry.userData.start != "undefined" &&
                       typeof obj.geometry.userData.end != "undefined"){
                      const start = obj.geometry.userData.start;
                      const end = obj.geometry.userData.end;
                      let seg = [];
                      if(keyword.length==1){
                        for(var i=0; i<start.length; i++){
                          if(start[i]<=keyword[0] && end[i] >= keyword[0]){
                            seg.push(i);
                          }
                        }
                      }else{
                        for(var i=0; i<start.length; i++){
                          if(start[i]>=keyword[0] && end[i] <= keyword[1]){
                            seg.push(i);
                          }
                        }
                      }
                      if(seg.length>0){
                        const instanceStart = obj.geometry.attributes.instanceStart.data.array;
                        for(var i=0; i<seg.length; i++){
                          pos.push(instanceStart[seg[i]*6]);
                          pos.push(instanceStart[seg[i]*6+1]);
                          pos.push(instanceStart[seg[i]*6+2]);
                          pos.push(instanceStart[seg[i]*6+3]);
                          pos.push(instanceStart[seg[i]*6+4]);
                          pos.push(instanceStart[seg[i]*6+5]);
                        }
                      }
                    }
                  }
                });
                this.sceneBottom2.traverse(obj => {
                  if(obj.isLine2 === true){
                    if(typeof obj.geometry.userData.start != "undefined" &&
                       typeof obj.geometry.userData.end != "undefined"){
                      const start = obj.geometry.userData.start;
                      const end = obj.geometry.userData.end;
                      let seg = [];
                      if(keyword.length==1){
                        for(var i=0; i<start.length; i++){
                          if(start[i]<=keyword[0] && end[i] >= keyword[0]){
                            seg.push(i);
                          }
                        }
                      }else{
                        for(var i=0; i<start.length; i++){
                          if(start[i]>=keyword[0] && end[i] <= keyword[1]){
                            seg.push(i);
                          }
                        }
                      }
                      if(seg.length>0){
                        const instanceStart = obj.geometry.attributes.instanceStart.data.array;
                        for(var i=0; i<seg.length; i++){
                          pos.push(instanceStart[seg[i]*6]);
                          pos.push(instanceStart[seg[i]*6+1]);
                          pos.push(instanceStart[seg[i]*6+2]);
                          pos.push(instanceStart[seg[i]*6+3]);
                          pos.push(instanceStart[seg[i]*6+4]);
                          pos.push(instanceStart[seg[i]*6+5]);
                        }
                      }
                    }
                  }
                });
                if(pos.length>=6){
                        const geometry = new LineSegmentsGeometry();
                        geometry.setPositions( pos );
                        const box = geometry.boundingBox;
                        this.camera2.position.set(
                          (box.max.x+box.min.x),
                          (box.max.y+box.min.y),
                          (box.max.z+box.min.z) );
                        this.animate();
                }
            }
          }
        }else{
          let gene_body = [];
          this.scene.traverse(obj =>  {
            if(obj.isCSS2DObject === true){
              if(obj.name==searchparam.keyword){
                result.push(obj);
              }
            }
            if(obj.isLine2 === true){
              if(obj.name==searchparam.keyword){
                gene_body.push(obj);
              }
            }
          });
          this.sceneBottom.traverse(obj =>  {
            if(obj.isCSS2DObject === true){
              if(obj.name==searchparam.keyword){
                result.push(obj);
              }
            }
            if(obj.isLine2 === true){
              if(obj.name==searchparam.keyword){
                gene_body.push(obj);
              }
            }
          });
          if(gene_body.length>0){
            const lineObj = gene_body[0];
            //lineObj.geometry.setColors([1, 0, 0]);
            const linewidth = lineObj.material.uniforms.linewidth.value;
            lineObj.material.uniforms.linewidth.value = 3*linewidth;
            const timeInterval = setInterval(function(){
              if(lineObj.material.uniforms.linewidth.value==linewidth){
                lineObj.material.uniforms.linewidth.value = 3*linewidth;
              }else{
                lineObj.material.uniforms.linewidth.value = linewidth;
              }
              this.animate();
            }.bind(this), 500);
            const timeOut = setTimeout(function(){
              clearInterval(timeInterval);
              lineObj.material.uniforms.linewidth.value = linewidth;
              this.animate();
            }.bind(this), 3000);
            const box = lineObj.geometry.boundingBox;
            this.camera.position.set(
              (box.max.x+box.min.x),
              (box.max.y+box.min.y),
              (box.max.z+box.min.z) );
            this.animate();
          }else{
            if(result.length>0){
              const pos = result[0].position;
              this.camera.position.set( pos.x, pos.y, pos.z );
              this.animate();
            }
          }
          if(this.sideBySide){
            result = [];
            gene_body = [];
            this.scene2.traverse(obj =>  {
              if(obj.isCSS2DObject === true){
                if(obj.name==searchparam.keyword){
                  result.push(obj);
                }
              }
              if(obj.isLine2 === true){
                if(obj.name==searchparam.keyword){
                  gene_body.push(obj);
                }
              }
            });
            this.sceneBottom2.traverse(obj =>  {
              if(obj.isCSS2DObject === true){
                if(obj.name==searchparam.keyword){
                  result.push(obj);
                }
              }
              if(obj.isLine2 === true){
                if(obj.name==searchparam.keyword){
                  gene_body.push(obj);
                }
              }
            });
            if(gene_body.length>0){
              const lineObj = gene_body[0];
              //lineObj.geometry.setColors([1, 0, 0]);
              const linewidth = lineObj.material.uniforms.linewidth.value;
              lineObj.material.uniforms.linewidth.value = 3*linewidth;
              const timeInterval = setInterval(function(){
                if(lineObj.material.uniforms.linewidth.value==linewidth){
                  lineObj.material.uniforms.linewidth.value = 3*linewidth;
                }else{
                  lineObj.material.uniforms.linewidth.value = linewidth;
                }
                this.animate();
              }.bind(this), 500);
              const timeOut = setTimeout(function(){
                clearInterval(timeInterval);
                lineObj.material.uniforms.linewidth.value = linewidth;
                this.animate();
              }.bind(this), 3000);
              const box = lineObj.geometry.boundingBox;
              this.camera2.position.set(
                (box.max.x+box.min.x),
                (box.max.y+box.min.y),
                (box.max.z+box.min.z) );
              this.animate();
            }else{
              if(result.length>0){
                const pos = result[0].position;
                this.camera2.position.set( pos.x, pos.y, pos.z );
                this.animate();
              }
            }
          }
        }
      }.bind(this)
    }
    searchGUI.add(searchparam, 'keyword').onChange(function(val){
      searchparam.keyword = val;
    }).onFinishChange(searchparam.search);
    searchGUI.add(searchparam, 'search');
    // animate GUI
    this.clock = new THREE.Clock();
    this.animateparam = {
      play : false,
      stepX : 0.3,
      stepY : 0.3,
      linked: true,
      up : false,
      down : false,
      left : false,
      right : false,
      ctrl : false,
      option : false
    };
    const animateGUI = this.gui.addFolder('animate');
    animateGUI.add(this.animateparam, 'play');
    animateGUI.add(this.animateparam, 'stepX', 0, 5 ).onChange( function ( val ) {
      this.animateparam.stepX = val;
    }.bind(this) );
    animateGUI.add(this.animateparam, 'stepY', 0, 5 ).onChange( function ( val ) {
      this.animateparam.stepY = val;
    }.bind(this) );
    this.gui.add(this.animateparam, 'linked').onChange( function(val){
      this.animateparam.linked = val;
    }.bind(this));
    // keyboard
    window.addEventListener("keydown", (event)=>{
      switch (event.keyCode) {
          case 87: // W
          case 38: // ArrowUp
            this.animateparam.up = true;
            break;
          case 65: // A
          case 37: // ArrowLeft
            this.animateparam.left = true;
            break;
          case 83: // S
          case 40: // ArrowDown
            this.animateparam.down = true;
            break;
          case 68: // D
          case 39: // ArrowRight
            this.animateparam.right = true;
            break;
          case 17: // Control
            this.animateparam.ctrl = true;
            break;
          case 18: // Alt
            this.animateparam.option = true;
        }
    });
    window.addEventListener("keyup", (event)=>{
      switch (event.keyCode) {
          case 87: // W
          case 38: // ArrowUp
            this.animateparam.up = false;
            break;
          case 65: // A
          case 37: // ArrowLeft
            this.animateparam.left = false;
            break;
          case 83: // S
          case 40: // ArrowDown
            this.animateparam.down = false;
            break;
          case 68: // D
          case 39: // ArrowRight
            this.animateparam.right = false;
            break;
          case 17: // Control
            this.animateparam.ctrl = false;
            break;
          case 18: // Alt
            this.animateparam.option = false;
        }
    });
        
    // link the cameras
    this.controls.addEventListener('change', () => {
      if(this.animateparam.linked){
        this.camera2.position.copy( this.camera.position );
        this.camera2.rotation.copy( this.camera.rotation );
        this.controls2.target.copy( this.controls.target );
      }
    });
    this.controls2.addEventListener('change', () => {
      if(this.animateparam.linked){
        this.camera.position.copy( this.camera2.position );
        this.camera.rotation.copy( this.camera2.rotation );
        this.controls.target.copy( this.controls2.target );
      }
    });
    
    animateGUI.close();
    
    // exporter GUI
    const saveBlob = (function(){
        const a = document.createElement('a');
        document.body.appendChild(a);
        a.style.display = 'none';
        return function saveData(blob, fileName) {
           const url = window.URL.createObjectURL(blob);
           a.href = url;
           a.download = fileName;
           a.click();
        };
    }());
    const expparam = {
      filename: 'threejsviewer',
      format: 'png',
      duration: 10,
      export : function() {
        let exporter;
        switch(expparam.format){
          case 'png':
            this.animate(false);
            this.renderer.domElement.toBlob(blob =>{
              saveBlob(blob, expparam.filename+'.'+expparam.format);
            });
            break;
          case 'drc':
            exporter = new DRACOExporter();
            const drcData = exporter.parse(this.scene, {exportColor:true});
            saveBlob(new Blob([drcData], {
              type: 'application/octet-stream'
            }), expparam.filename+'.'+expparam.format);
            break;
          case 'svg':
            var rendererSVG = new SVGRenderer();
            rendererSVG.setSize(this.width, this.height);
            rendererSVG.setClearColor( this.background, this.bckalpha );
            rendererSVG.render(this.scene, this.camera);
            var XMLS = new XMLSerializer();
            var svgData = XMLS.serializeToString(rendererSVG.domElement);
            var preface = '<?xml version="1.0" standalone="no"?>\r\n';
            var blob = new Blob([preface, svgData], {
              type: "image/svg+xml;charset=utf-8"
            });
            saveBlob(blob, expparam.filename+'.'+expparam.format);
            break;
          case 'gltf':
            exporter = new GLTFExporter();
            exporter.parse(
              this.scene,
              function(gltf){
                var blob = new Blob([JSON.stringify(gltf)], {
                  type: 'text/plain'
                });
                saveBlob(blob, expparam.filename+'.'+expparam.format);
              },
              function(error){
                console.log(error);
              }
            )
            break;
          case 'ply':
            exporter = new PLYExporter();
            const plyData = exporter.parse(this.scene);
            saveBlob(new Blob([plyData], {
              type: 'text/plain'
            }), expparam.filename+'.'+expparam.format);
            break;
          case 'stl':
            exporter = new STLExporter();
            const stlData = exporter.parse( this.scene );
            saveBlob(new Blob([stlData], {
              type: 'text/plain'
            }), expparam.filename+'.'+expparam.format);
            break;
          case 'video':
            if(this.animateparam.play){
              const stream = this.renderer.domElement.captureStream(25);
              var recordedChunks = [];
              var options = {};
              var mediaRecorder = new MediaRecorder(stream, options);
              function handleDataAvailable(event) {
                recordedChunks.push(event.data);
              }
              mediaRecorder.ondataavailable = handleDataAvailable;
              var timeout, counter=expparam.duration;
              var countdown = function(counter){
                timeout = setInterval(()=>{
                      counter -= 1;
                      exporterBotton.name('export in '+counter+'s');
                  },1000);
              };
              var animationLoop = function (){
                countdown(counter);
                // while we're recording
                if (mediaRecorder.state !== "inactive") {
                  requestAnimationFrame(this.animate);
                }
              }.bind(this);
              mediaRecorder.onstart = animationLoop;
              mediaRecorder.start();
              var animationStop = function (){
                clearInterval(timeout);
                counter = expparam.duration;
                exporterBotton.name('export');
                saveBlob(new Blob(recordedChunks, {
                type: 'video/webm'
              }), expparam.filename+'.webm');
              }
              mediaRecorder.onstop = animationStop;
              setTimeout(()=>{
                mediaRecorder.stop();
              }, 1000*expparam.duration);
            }else{
              alert('Please turn on the animate first.');
            }
            break;
          default:
            alert('not support yet!');
        }
      }.bind(this)
    };
    const exporterGUI = this.gui.addFolder('exporter');
    exporterGUI.add(expparam, 'filename').onChange(
      val => expparam.filename = val
    );
    var availableFormat = ['drc', 'gltf', 'ply', 'png', 'stl', 'svg', 'video'];
    var supportFormat = ['png', 'video'];
    var exporterDuration = exporterGUI.add(expparam, 'duration', 0, 120, 1).onChange(
      val => expparam.duration = val
    ).hide();
    exporterGUI.add(expparam, 'format', supportFormat).onChange(
      val => {
        expparam.format = val;
        if(val=='video'){
          exporterDuration.show();
        }else{
          exporterDuration.hide();
        }
      }
    );
    const exporterBotton = exporterGUI.add(expparam, 'export');
    exporterGUI.close();
    this.layer = {};
    
    // background color gui
    this.bckcolparam = {
      color: this.background,
      alpha: this.bckalpha,
      bottomColor: this.backgroundBottom,
      bottomAlpha: this.bckalphaBottom,
      rightColor: this.background2,
      rightAlpha: this.bckalpha2,
      bottomRight: this.backgroundBottom2,
      bottomRightAlhpa: this.bckalphaBottom2
    }
    this.bckcolGUI = this.gui.addFolder('background colors');
    this.bckcolGUI.addColor(this.bckcolparam, 'color').onChange( val =>{
      this.bckcolparam.color = new THREE.Color(val);
      this.background = new THREE.Color(val);
      this.scene.background = new THREE.Color(
            this.background.r * this.bckalpha,
            this.background.g * this.bckalpha,
            this.background.b * this.bckalpha
          );
    });
    this.bckcolGUI.add(this.bckcolparam, 'alpha', 0, 1).onChange( val =>{
      this.bckcolparam.alpha = val;
      this.bckalpha = val;
      this.scene.background = new THREE.Color(
            this.background.r * this.bckalpha,
            this.background.g * this.bckalpha,
            this.background.b * this.bckalpha
          );
    });
    this.bckcolGUI.addColor(this.bckcolparam, 'bottomColor').onChange( val =>{
      this.bckcolparam.bottomColor = new THREE.Color(val);
      this.backgroundBottom = new THREE.Color(val);
      this.sceneBottom.background = new THREE.Color(
            this.backgroundBottom.r * this.bckalphaBottom,
            this.backgroundBottom.g * this.bckalphaBottom,
            this.backgroundBottom.b * this.bckalphaBottom
          );
    }).hide();
    this.bckcolGUI.add(this.bckcolparam, 'bottomAlpha', 0, 1).onChange( val =>{
      this.bckcolparam.bottomAlpha = val;
      this.bckalphaBottom = val;
      this.sceneBottom.background = new THREE.Color(
            this.backgroundBottom.r * this.bckalphaBottom,
            this.backgroundBottom.g * this.bckalphaBottom,
            this.backgroundBottom.b * this.bckalphaBottom
          );
    }).hide();
    this.bckcolGUI.addColor(this.bckcolparam, 'rightColor').onChange( val =>{
      this.bckcolparam.rightColor = new THREE.Color(val);
      this.background2 = new THREE.Color(val);
      this.scene2.background = new THREE.Color(
            this.background2.r * this.bckalpha2,
            this.background2.g * this.bckalpha2,
            this.background2.b * this.bckalpha2
          );
    }).hide();
    this.bckcolGUI.add(this.bckcolparam, 'rightAlpha', 0, 1).onChange( val =>{
      this.bckcolparam.rightAlpha = val;
      this.bckalpha2 = val;
      this.scene2.background = new THREE.Color(
            this.background2.r * this.bckalpha2,
            this.background2.g * this.bckalpha2,
            this.background2.b * this.bckalpha2
          );
    }).hide();
    this.bckcolGUI.addColor(this.bckcolparam, 'bottomRight').onChange( val =>{
      this.bckcolparam.bottomRight = new THREE.Color(val);
      this.backgroundBottom2 = new THREE.Color(val);
      this.sceneBottom2.background = new THREE.Color(
            this.backgroundBottom2.r * this.bckalphaBottom2,
            this.backgroundBottom2.g * this.bckalphaBottom2,
            this.backgroundBottom2.b * this.bckalphaBottom2
          );
    }).hide();
    this.bckcolGUI.add(this.bckcolparam, 'bottomRightAlhpa', 0, 1).onChange( val =>{
      this.bckcolparam.bottomRightAlhpa = val;
      this.bckalphaBottom2 = val;
      this.sceneBottom2.background = new THREE.Color(
            this.backgroundBottom2.r * this.bckalphaBottom2,
            this.backgroundBottom2.g * this.bckalphaBottom2,
            this.backgroundBottom2.b * this.bckalphaBottom2
          );
    }).hide();
    this.bckcolGUI.close();
    
    //soft white light
    const ambientLight = new THREE.AmbientLight( 0x404040, 1);
    this.scene.add( ambientLight );
    const ambientparams = {
      AmbientColor: ambientLight.color.getHex(),
      AmbientIntensity: ambientLight.intensity
    };
    
    // spotlight GUI
    let directionalLight; 
    directionalLight = new THREE.DirectionalLight( 0xffffff, 5 );
    directionalLight.position.set( 2.5, 5, -25 );
    this.scene.add( directionalLight );
    
    const lightparams = {
      color: directionalLight.color.getHex(),
      intensity: directionalLight.intensity,
      x: 2.5,
      y: 5,
      z: -25,
      setPosition: function(){
        directionalLight.position.set(lightparams.x, lightparams.y, lightparams.z);
      }
    };
    const spotlightGUI = this.gui.addFolder('light settings');
    
    spotlightGUI.addColor( ambientparams, 'AmbientColor' ).onChange( function ( val ) {
      ambientLight.color.setHex( val );
    } );
    spotlightGUI.add( ambientparams, 'AmbientIntensity', 0, 10 ).onChange( function ( val ) {
      ambientLight.intensity = val;
    } );
    
    spotlightGUI.addColor( lightparams, 'color' ).onChange( function ( val ) {
      directionalLight.color.setHex( val );
    } );
    spotlightGUI.add( lightparams, 'intensity', 0, 100 ).onChange( function ( val ) {
      directionalLight.intensity = val;
    } );
    spotlightGUI.add( lightparams, 'x', -50, 50 ).onChange( function ( val ) {
      lightparams.x = val;
      lightparams.setPosition();
    } );
    spotlightGUI.add( lightparams, 'y', -50, 50 ).onChange( function ( val ) {
      lightparams.y = val;
      lightparams.setPosition();
    } );
    spotlightGUI.add( lightparams, 'z', -50, 50 ).onChange( function ( val ) {
      lightparams.z = val;
      lightparams.setPosition();
    } );
    spotlightGUI.close();
  }
  
  getLayer(tag){
    return(this.layer[tag]);
  }
  
  setResizeBlockPos(){
    this.resizeBlock.style.top = this.container.offsetTop + this.height - 5 + 'px';
    this.resizeBlock.style.left = this.container.offsetLeft + this.width - 5 + 'px';
  }
  
  initSlider() {
    this.slider.style.display = 'block';
    this.slider.style.top = this.container.getBoundingClientRect().top +
        this.sliderPos + 'px';
    var onPointerDown = function() {
      if ( event.isPrimary === false ) return;
      this.controls.enabled = false;
      this.controls2.enabled = false;
      window.addEventListener( 'pointermove', onPointerMove );
      window.addEventListener( 'pointerup', onPointerUp );
    }.bind(this);
    
    var onPointerUp = function() {
      this.controls.enabled = true;
      this.controls2.enabled = true;
      window.removeEventListener( 'pointermove', onPointerMove );
      window.removeEventListener( 'pointerup', onPointerUp );
    }.bind(this)
    
    var onPointerMove = function ( e ) {
      if ( event.isPrimary === false ) return;
      var offset = this.container.getBoundingClientRect().top;
      this.sliderPos = Math.max( 0, Math.min( this.height, e.pageY -
        offset ) );
      this.slider.style.top = this.sliderPos + offset + 'px';
    }.bind(this);
    
    this.slider.style.touchAction = 'none'; // disable touch scroll
    this.slider.addEventListener( 'pointerdown', onPointerDown );
  }
  
  create_plot(x){
    //console.log(x);
    //const twoPi = Math.PI * 2;
    //x is a named array
    if('background' in x){
      //Separate RGB values between 0 and 1
      this.background = new THREE.Color(
        x.background.r[0],
        x.background.g[0],
        x.background.b[0]
      );
      this.bckalpha = x.background.alpha[0];
      this.bckcolparam.color = this.background;
      this.bckcolparam.alpha = this.bckalpha;
      this.bckcolGUI.controllers[0].setValue(this.background);
      this.bckcolGUI.controllers[1].setValue(this.bckalpha);
      this.background2 = new THREE.Color(
        x.background.r[2],
        x.background.g[2],
        x.background.b[2]
      );
      this.bckalpha2 = x.background.alpha[2];
      this.bckcolparam.rightColor = this.background2;
      this.bckcolparam.rightAlpha = this.bckalpha2;
      this.bckcolGUI.controllers[4].setValue(this.background2);
      this.bckcolGUI.controllers[5].setValue(this.bckalpha2);
    }
    if('maxRadius' in x){
      this.maxRadius = x.maxRadius;
    }
    if('maxLineWidth' in x){
      this.maxLineWidth = x.maxLineWidth;
    }
    if('sideBySide' in x){
      this.sideBySide = x.sideBySide;
      if(x.sideBySide){
        this.camera.aspect = 2*this.width/this.height;
        this.camera2.aspect = 2*this.width/this.height;
        this.camera.updateProjectionMatrix();
        this.camera2.updateProjectionMatrix();
        this.labelRenderer.setSize( this.width/2, this.height );
        this.labelRenderer2.setSize( this.width/2, this.height );
        this.labelRenderer2.domElement.style.left = this.width/2+'px';
        this.container.insertBefore(this.labelRenderer2.domElement, this.slider);
        this.bckcolGUI.controllers[4].show();
        this.bckcolGUI.controllers[5].show();
      }
    }
    if('overlay' in x){
      this.overlay = x.overlay;
    }
    if(x.overlay){
        this.initSlider();
        if('background' in x){
          this.scene.background = new THREE.Color(
            x.background.r[0] * this.bckalpha,
            x.background.g[0] * this.bckalpha,
            x.background.b[0] * this.bckalpha
          );
          this.backgroundBottom = new THREE.Color(
            x.background.r[1],
            x.background.g[1],
            x.background.b[1]
          );
          this.bckalphaBottom = x.background.alpha[1];
          this.sceneBottom.background = new THREE.Color(
            x.background.r[1] * this.bckalphaBottom,
            x.background.g[1] * this.bckalphaBottom,
            x.background.b[1] * this.bckalphaBottom
          );
          this.bckcolparam.bottomColor = this.backgroundBottom;
          this.bckcolparam.bottomAlpha = this.bckalphaBottom;
          this.bckcolGUI.controllers[2].setValue(this.backgroundBottom).show();
          this.bckcolGUI.controllers[3].setValue(this.bckalphaBottom).show();
          this.scene2.background = new THREE.Color(
            x.background.r[2] * this.bckalpha2,
            x.background.g[2] * this.bckalpha2,
            x.background.b[2] * this.bckalpha2
          );
          this.backgroundBottom2 = new THREE.Color(
            x.background.r[3],
            x.background.g[3],
            x.background.b[3]
          );
          this.bckalphaBottom2 = x.background.alpha[3];
          this.sceneBottom2.background = new THREE.Color(
            x.background.r[3] * this.bckalphaBottom2,
            x.background.g[3] * this.bckalphaBottom2,
            x.background.b[3] * this.bckalphaBottom2
          );
          this.bckcolparam.bottomRight = this.backgroundBottom2;
          this.bckcolparam.bottomRightAlpha = this.bckalphaBottom2;
          this.bckcolGUI.controllers[6].setValue(this.backgroundBottom2);
          this.bckcolGUI.controllers[7].setValue(this.bckalphaBottom2);
          if(this.sideBySide){
            this.bckcolGUI.controllers[6].show();
            this.bckcolGUI.controllers[7].show();
          }
        }
    }
    
    const arrowLayer = [];
    const groupFolder = this.gui.addFolder('Group setting');
    const groupFolderObj = {};
    const groupParamObj = {};
    if('taglayers' in x){
      const labelLayer = {};
      const layerFolder = groupFolder.addFolder('show/hide');
      var lay=0;
      if(!Array.isArray(x.taglayers)){
        x.taglayers=[x.taglayers];
      }
      for(var i=0; i<x.taglayers.length&&i<32; i++){
        lay = x.taglayers[i];
        this.layer[lay] = i+1;
        labelLayer['Toggle '+lay]=function(tag){
          this.camera.layers.toggle(this.getLayer(tag));
          if(this.sideBySide){
            this.camera2.layers.toggle(this.getLayer(tag));
          }
        }.bind(this, lay);
        layerFolder.add(labelLayer, 'Toggle '+lay);
      }
      labelLayer['Toggle all arrows']=function(){
        for(var i=0; i<arrowLayer.length; i++){
          arrowLayer[i].visible = !arrowLayer[i].visible;
        }
      }
      layerFolder.add(labelLayer, 'Toggle all arrows');
    }
    if('tagWithChild' in x){
      if(!Array.isArray(x.tagWithChild)){
        x.tagWithChild = [x.tagWithChild];
      }
    }else{
      x.tagWithChild = [];
    }
    function updateGroupGeometry(mesh, geometry){
            mesh.geometry.dispose();
            mesh.geometry = geometry;
        }
    function initNewMeshPos(obj, ele, offset={x:0,y:0,z:0}){
      const matrix = new THREE.Matrix4();
      for ( let i = 0; i < obj.count; i ++ ) {
        matrix.setPosition( ele.positions[i*3] + offset.x,
                            ele.positions[i*3+1] + offset.y,
                            ele.positions[i*3+2] + offset.z );
        obj.setMatrixAt( i, matrix );
      }
    }
    function initNewMesh(obj, ele, offset={x:0,y:0,z:0}){
      const matrix = new THREE.Matrix4();
      const color = new THREE.Color();
      for ( let i = 0; i < obj.count; i ++ ) {
        matrix.setPosition( ele.positions[i*3] + offset.x,
                            ele.positions[i*3+1] + offset.y,
                            ele.positions[i*3+2] + offset.z );
        obj.setMatrixAt( i, matrix );
        if(ele.colors.length==ele.positions.length){
          obj.setColorAt( i, color.setRGB(
                ele.colors[i*3],
                ele.colors[i*3+1],
                ele.colors[i*3+2]
              ) );
        }else{//same color for alll elements
          obj.setColorAt( i, color.setRGB(
                ele.colors[0],
                ele.colors[1],
                ele.colors[2]
              ) );
        }
      }
    }
    function getGUIbyName(name, fld){
          const id = fld.children.forEach((it, id)=>{
            if(it._name==name) return(id);
          });
          return(typeof id == 'undefined');
    }
    // each element 
    for(var k in x){
      if(k!='background' && k!='maxRadius' &&
         k!='maxLineWidth' && k!='taglayers' &&
         k!='tagWithChild' &&
         k!='overlay' && k!='sideBySide'){
        let ele = x[k];
        const param = {
          'size': 0.08,
          'radius': 0.08,
          'radiusTop': 0.08,
          'radiusBottom': 0.08,
          'tube': 0.08,
          'width':0.08,
          'height':0.08,
          'depth':0.08,
          'opacity':1,
          'transparent':true,
          'color': new THREE.Color(
                ele.colors[0],
                ele.colors[1],
                ele.colors[2]),
          'thetaStart': 0,
          'thetaLength': 2*Math.PI
        };
        const len = ele.positions.length/3;
        if(typeof groupFolderObj[ele.tag] == 'undefined'){
          groupFolderObj[ele.tag] = this.gui.addFolder(ele.tag);
          groupParamObj[ele.tag] = param;
          for(var key in param){
            if(getGUIbyName(key, groupFolderObj[ele.tag]) &&
            x.tagWithChild.indexOf(ele.tag) != -1){
              if(ele.hasOwnProperty(key)){
                groupParamObj[ele.tag][key] = 0;
                switch(key){
                  case 'size':
                    groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange((val) => {
                        groupParamObj[ele.tag].size = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              if(obj.isLine2 || obj.isLineSegments2){
                                obj.material.linewidth += val;
                              }
                            }
                          }else{
                            if(obj.isCSS2DObject){
                              if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                                if(typeof obj.element.style.fontSize == 'undefined'){
                                  obj.element.style.fontSize = 6+val+'px';
                                  console.log(obj.element.style.fontSize);
                                }else{
                                  obj.element.style.fontSize = parseFloat(obj.element.style.fontSize) + val+'px';
                                }
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase size by:');
                    break;
                  case 'radius':
                    groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].radius = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              switch(obj.geometry.type){
                                case 'SphereGeometry':
                                  updateGroupGeometry(obj, new THREE.SphereGeometry(
                                    obj.geometry.parameters.radius + val, 32, 16));
                                  break;
                                case 'CapsuleGeometry':
                                  updateGroupGeometry(obj, new THREE.CapsuleGeometry(
                                    obj.geometry.parameters.radius + val, obj.geometry.parameters.height));
                                  break;
                                case 'ConeGeometry':
                                  updateGroupGeometry(obj, new THREE.ConeGeometry(
                                    obj.geometry.parameters.radius + val, obj.geometry.parameters.height));
                                  break;
                                case 'DodecahedronGeometry':
                                  updateGroupGeometry(obj, new THREE.DodecahedronGeometry(
                                    obj.geometry.parameters.radius + val));
                                  break;
                                case 'IcosahedronGeometry':
                                  updateGroupGeometry(obj, new THREE.IcosahedronGeometry(
                                    obj.geometry.parameters.radius + val));
                                  break;
                                case 'OctahedronGeometry':
                                  updateGroupGeometry(obj, new THREE.OctahedronGeometry(
                                    obj.geometry.parameters.radius + val));
                                  break;
                                case 'TetrahedronGeometry':
                                  updateGroupGeometry(obj, new THREE.TetrahedronGeometry(
                                    obj.geometry.parameters.radius + val));
                                  break;
                                case 'TorusGeometry':
                                  updateGroupGeometry(obj, new THREE.TorusGeometry(
                                    obj.geometry.parameters.radius + val, obj.geometry.parameters.tube));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase radius by:');
                      break;
                    case 'radiusTop':
                      groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].radiusTop = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              console.log(obj);
                              switch(obj.geometry.type){
                                case 'CylinderGeometry':
                                  updateGroupGeometry(obj, new THREE.CylinderGeometry(
                                    obj.geometry.parameters.radiusTop + val,
                                    obj.geometry.parameters.radiusBottom,
                                    obj.geometry.parameters.height
                                  ));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase radiusTop by:');
                      break;
                    case 'radiusBottom':
                      groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].radiusBottom = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              switch(obj.geometry.type){
                                case 'CylinderGeometry':
                                  updateGroupGeometry(obj, new THREE.CylinderGeometry(
                                    obj.geometry.parameters.radiusTop,
                                    obj.geometry.parameters.radiusBottom + val,
                                    obj.geometry.parameters.height
                                  ));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase radiusBottom by:');
                      break;
                    case 'width':
                      groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].width = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              switch(obj.geometry.type){
                                case 'BoxGeometry':
                                  updateGroupGeometry(obj, new THREE.BoxGeometry(
                                      obj.geometry.parameters.width + val, 
                                      obj.geometry.parameters.height,
                                      obj.geometry.parameters.depth));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase width by:');
                      break;
                    case 'height':
                      groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].height = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              switch(obj.geometry.type){
                                case 'BoxGeometry':
                                  updateGroupGeometry(obj, new THREE.BoxGeometry(
                                      obj.geometry.parameters.width, 
                                      obj.geometry.parameters.height + val,
                                      obj.geometry.parameters.depth));
                                  break;
                                case 'CapsuleGeometry':
                                  updateGroupGeometry(obj, new THREE.CapsuleGeometry(obj.geometry.parameters.radius, obj.geometry.parameters.height + val));
                                  break;
                                case 'ConeGeometry':
                                  updateGroupGeometry(obj, new THREE.ConeGeometry(obj.geometry.parameters.radius, obj.geometry.parameters.height + val));
                                  break;
                                case 'CylinderGeometry':
                                  updateGroupGeometry(obj, new THREE.CylinderGeometry(
                                    obj.geometry.parameters.radiusTop,
                                    obj.geometry.parameters.radiusBottom,
                                    obj.geometry.parameters.height + val
                                  ));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase height by:');
                      break;
                    case 'depth':
                      groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].depth = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              switch(obj.geometry.type){
                                case 'BoxGeometry':
                                  updateGroupGeometry(obj, new THREE.BoxGeometry(
                                      obj.geometry.parameters.width, 
                                      obj.geometry.parameters.height,
                                      obj.geometry.parameters.depth + val));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase depth by:');
                      break;
                  case 'tube':
                    groupFolderObj[ele.tag].add(
                      groupParamObj[ele.tag], key, -10, 10, .5)
                      .onFinishChange(val => {
                        groupParamObj[ele.tag].tube = val;
                        var traverseFun = function(obj){
                          if(obj.isMesh){
                            if(obj.layers.mask == Math.pow(2, this.getLayer(ele.tag))){
                              switch(obj.geometry.type){
                                case 'TorusGeometry':
                                 updateGroupGeometry(obj, new THREE.TorusGeometry(obj.geometry.parameters.radius, obj.geometry.parameters.tube + val));
                                  break;
                                default:
                                  console.log(obj);
                              }
                            }
                          }
                        }.bind(this);
                        this.objects.traverse(traverseFun);
                        this.objectsBottom.traverse(traverseFun);
                        if(this.sideBySide){
                          this.objects2.traverse(traverseFun);
                          this.objectsBottom2.traverse(traverseFun);
                        }
                      }).name('increase tube by:');
                      break;
                }
              }else{
                switch(key){
                    case 'opacity':
                      groupFolderObj[ele.tag].add(
                        groupParamObj[ele.tag], key, 0, 1)
                        .onChange(val=>{
                          groupParamObj[ele.tag] = val;
                          var traverseFun = function(obj){
                            if(obj.isMesh){
                              if(obj.layers.mask==Math.pow(2, this.getLayer(ele.tag))){
                                obj.material.opacity = val;
                              }
                            }
                          }.bind(this);
                          this.objects.traverse(traverseFun);
                          this.objectsBottom.traverse(traverseFun);
                          if(this.sideBySide){
                            this.objects2.traverse(traverseFun);
                            this.objectsBottom2.traverse(traverseFun);
                          }
                        });
                      break;
                    case 'transparent':
                      groupFolderObj[ele.tag].add(
                        groupParamObj[ele.tag], key)
                        .onChange(val=>{
                          groupParamObj[ele.tag] = val;
                          var traverseFun = function(obj){
                            if(obj.isMesh){
                              if(obj.layers.mask==Math.pow(2, this.getLayer(ele.tag))){
                                obj.material.transparent = val;
                              }
                            }
                          }.bind(this);
                          this.objects.traverse(traverseFun);
                          this.objectsBottom.traverse(traverseFun);
                          if(this.sideBySide){
                            this.objects2.traverse(traverseFun);
                            this.objectsBottom2.traverse(traverseFun);
                          }
                        });
                      break;
                }
              }
            }
          }
        }
        var folder = groupFolderObj[ele.tag].addFolder(ele.type+' '+k);
        let geometry = new THREE.BufferGeometry();
        let obj = new THREE.InstancedMesh();
        let material = new THREE.MeshStandardMaterial( {
              color: 0xffffff,
              opacity: 1,
              transparent: true,
              metalness: 0,
              roughness: 0
            } );
        switch(ele.type){
          case 'arrow':
            const hex = new THREE.Color(
                ele.colors[0],
                ele.colors[1],
                ele.colors[2]);
            const arrowDirection = new THREE.Vector3();
            arrowDirection.subVectors(this.scene.position,
                new THREE.Vector3( // next three elements are end x,y,z
                ele.positions[0]-ele.positions[3],
                ele.positions[1]-ele.positions[4],
                ele.positions[2]-ele.positions[5]
                )).normalize();
            obj = new THREE.ArrowHelper(
              arrowDirection,
              new THREE.Vector3(
                ele.positions[0],
                ele.positions[1],
                ele.positions[2]
              ), // first three elements are start x,y,z
              ele.size/100, '#'+hex.getHexString(), ele.headLength/5, ele.headWidth/10);
            obj.layers.set(this.getLayer(ele.tag));
            arrowLayer.push(obj);
            break;
          case 'line':// Line2 ( LineGeometry, LineMaterial )
            param.size = ele.size;
            geometry = new LineGeometry();
            geometry.setPositions( ele.positions );
            if(ele.colors.length!=ele.positions.length){
              // single colors
              for(var i=1; i<len; i++){
                ele.colors.push(ele.colors[0]);
                ele.colors.push(ele.colors[1]);
                ele.colors.push(ele.colors[2]); 
              }
            }
            geometry.setColors( ele.colors );
            // coordinates
            if(Array.isArray(ele.target) && ele.target.length==len-1){
              var start = [];
              var end = [];
              for(var i=0; i<len-1; i++ ){
                var s = ele.target[i].split('-');
                if(s.length==2){
                  start.push(parseInt(s[0]));
                  end.push(parseInt(s[1]));
                }
              }
              if(start.length==len-1){
                geometry.userData = {
                  'start':start,
                  'end': end
                };
              }
            }
            material = new LineMaterial( {
              color: 0xffffff,
              linewidth: ele.size, // in world units with size attenuation, pixels otherwise
              vertexColors: true,
              dashed: false,
              alphaToCoverage: false,
              opacity: 1,
              transparent: true
            } );
            obj = new Line2( geometry, material );
            obj.computeLineDistances();
            obj.scale.set( 1, 1, 1 );
            obj.layers.set(this.getLayer(ele.tag));
            if(typeof ele.target === 'string' || ele.target instanceof String){
              // gene label
              obj.name = ele.target;
            }
            folder.add(param, 'size', 0, this.maxLineWidth).onChange( function( val) {
              material.linewidth = val;
            });
            break;
          case 'segment':
            param.size = ele.size;
            if(ele.alpha != null){
              param.opacity = ele.alpha
            }
            geometry = new LineSegmentsGeometry();
            geometry.setPositions( ele.positions );
            if(ele.colors.length!=ele.positions.length){
              // single colors
              for(var i=1; i<len; i++){
                ele.colors.push(ele.colors[0]);
                ele.colors.push(ele.colors[1]);
                ele.colors.push(ele.colors[2]); 
              }
            }
            geometry.setColors( ele.colors );
            material = new LineMaterial({ 
              color: 0xffffff,
              linewidth: param.size,
              vertexColors: true,
              opacity: param.opacity
            });
            obj = new LineSegments2(geometry, material);
            obj.layers.set(this.getLayer(ele.tag));
            folder.add(param, 'size', 0, this.maxLineWidth).onChange( function( val){
              material.linewidth = val;
            });
            param.opacity = 1;
            break;
          case 'sphere':
            param.radius = ele.radius;
            const spheredata = {
              radius: ele.radius,
              widthSegments: 32, //3-64
              heightSegments: 16//2-32
            };
            geometry = new THREE.SphereGeometry(
              spheredata.radius, spheredata.widthSegments, spheredata.heightSegments);
            obj = new THREE.InstancedMesh( geometry, material, len );
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function( val) {
              spheredata.radius = val;
              updateGroupGeometry(obj, new THREE.SphereGeometry(
                spheredata.radius,
                spheredata.widthSegments,
                spheredata.heightSegments));
            });
            break;
          case 'box':
            param.width = ele.width;
            param.height = ele.height;
            param.depth = ele.depth;
            const boxdata = {
              width : ele.width,
              height : ele.height,
              depth : ele.depth
            }
            geometry = new THREE.BoxGeometry(
              boxdata.width, 
              boxdata.height,
              boxdata.depth);
            obj = new THREE.InstancedMesh( geometry, material, len );
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            function updateBoxGeometry(){
              updateGroupGeometry(obj, new THREE.BoxGeometry(
                boxdata.width, 
                boxdata.height,
                boxdata.depth));
            }
            folder.add(param, 'width', 0, this.maxRadius).onChange( function( val) {
              boxdata.width = val;
              updateBoxGeometry();
            });
            folder.add(param, 'height', 0, this.maxRadius).onChange( function( val) {
              boxdata.height = val;
              updateBoxGeometry();
            });
            folder.add(param, 'depth', 0, this.maxRadius).onChange( function( val) {
              boxdata.depth = val;
              updateBoxGeometry();
            });
            break;
          case 'capsule':
            param.radius = ele.radius;
            param.height = ele.height;
            const capsuledata = {
              radius : ele.radius,
              height : ele.height
            }
            geometry = new THREE.CapsuleGeometry(
              capsuledata.radius,
              capsuledata.height
            );
            obj = new THREE.InstancedMesh( geometry, material, len);
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            function updateCapsuleGeometry(){
              updateGroupGeometry(obj, new THREE.CapsuleGeometry(
                capsuledata.radius,
                capsuledata.size
              ))
            }
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function(val) {
              capsuledata.radius = val;
              updateCapsuleGeometry()
            });
            folder.add(param, 'height', 0, this.maxRadius).onChange( function(val) {
              capsuledata.height = val;
              updateCapsuleGeometry()
            });
            break;
          case 'circle':
            param.radius = ele.radius;
            param.thetaStart = ele.thetaStart;
            param.thetaLength = ele.thetaLength;
            const circledata = {
              radius : ele.radius,
              thetaStart : ele.thetaStart,
              thetaLength : ele.thetaLength
            }
            geometry = new THREE.CircleGeometry(
              circledata.radius,
              32,
              circledata.thetaStart,
              circledata.thetaLength
            );
            obj = new THREE.InstancedMesh( geometry, material, len);
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            function updateCircleGeometry(){
              updateGroupGeometry(obj, new THREE.CircleGeometry(
                circledata.radius,
                32,
                circledata.thetaStart,
                circledata.thetaLength
              ));
            }
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function(val) {
              circledata.radius = val;
              updateCircleGeometry()
            });
            folder.add(param, 'thetaStart', 0, 2*Math.PI).onChange( function(val) {
              circledata.thetaStart = val;
              updateCircleGeometry()
            });
            folder.add(param, 'thetaLength', 0, 2*Math.PI).onChange( function(val) {
              circledata.thetaLength = val;
              updateCircleGeometry()
            });
            break;
          case 'cone':
            param.radius = ele.radius;
            param.height = ele.height;
            const conedata = {
              radius: ele.radius,
              height: ele.height
            }
            geometry = new THREE.ConeGeometry(
              conedata.radius,
              conedata.height
            );
            obj = new THREE.InstancedMesh( geometry, material, len);
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            function updateConeGeometry(){
              updateGroupGeometry(obj, new THREE.ConeGeometry(
                conedata.radius,
                conedata.height
              ));
            }
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function(val) {
              conedata.radius = val;
              updateConeGeometry()
            });
            folder.add(param, 'height', 0, this.maxRadius).onChange( function(val) {
              conedata.height = val;
              updateConeGeometry()
            });
            break;
          case 'cylinder':
            param.radiusTop = ele.radiusTop;
            param.radiusBottom = ele.radiusBottom;
            param.height = ele.height;
            const cylinderdata = {
              radiusTop: ele.radiusTop,
              radiusBottom: ele.radiusBottom,
              height: ele.height
            }
            geometry = new THREE.CylinderGeometry(
              cylinderdata.radiusTop,
              cylinderdata.radiusBottom,
              cylinderdata.height
            )
            obj = new THREE.InstancedMesh( geometry, material, len);
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            function updateCylinderGeometry(){
              updateGroupGeometry(obj, new THREE.CylinderGeometry(
              cylinderdata.radiusTop,
              cylinderdata.radiusBottom,
              cylinderdata.height
              ))
            }
            folder.add(param, 'radiusTop', 0, this.maxRadius).onChange( function(val) {
              cylinderdata.radiusTop = val;
              updateCylinderGeometry()
            });
            folder.add(param, 'radiusBottom', 0, this.maxRadius).onChange( function(val) {
              cylinderdata.radiusBottom = val;
              updateCylinderGeometry()
            });
            folder.add(param, 'height', 0, this.maxRadius).onChange( function(val) {
              cylinderdata.height = val;
              updateCylinderGeometry()
            });
            break;
          case 'dodecahedron':
            param.radius = ele.radius;
            const dodecahedrondata = {
              radius: ele.radius
            };
            geometry = new THREE.DodecahedronGeometry(
              dodecahedrondata.radius);
            obj = new THREE.InstancedMesh( geometry, material, len );
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function( val) {
              dodecahedrondata.radius = val;
              updateGroupGeometry(obj, new THREE.DodecahedronGeometry(
                dodecahedrondata.radius));
            });
            break;
          case 'icosahedron':
            param.radius = ele.radius;
            const icosahedrondata = {
              radius: ele.radius
            };
            geometry = new THREE.IcosahedronGeometry(
              icosahedrondata.radius);
            obj = new THREE.InstancedMesh( geometry, material, len );
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function( val) {
              icosahedrondata.radius = val;
              updateGroupGeometry(obj, new THREE.IcosahedronGeometry(
                icosahedrondata.radius));
            });
            break;
          case 'json':
            const loader = new THREE.BufferGeometryLoader();
            geometry = loader.parse(ele.json);
            geometry.computeVertexNormals();
            obj = new THREE.InstancedMesh( geometry, material, len );
            obj.layers.set(this.getLayer(ele.tag));
            initNewMesh(obj, ele);
            break;
          case 'label'://ask to modify the CSS2DRenderer.js at the line
                      //const visible = ( _vector.z >= - 1 && _vector.z <= 1 ) && ( object.layers.test( camera.layers ) === true );
                      // to const visible = object.layers.test( camera.layers ) === true;
                      // becase the _vector.x,y,z always is null. no global viewport.
            let labelDiv = document.createElement('div');
            let css2obj = new CSS2DObject();
            const color = new THREE.Color();
            labelDiv.style.backgroundColor = 'transparent';
            if(!Array.isArray(ele.label)){
              ele.label = [ele.label];
            }
            for(var i=0; i<ele.label.length; i++){
              labelDiv.textContent = ele.label;
              if(ele.colors.length==ele.positions.length){
                labelDiv.style.color='#'+color.setRGB(
                      ele.colors[i*3],
                      ele.colors[i*3+1],
                      ele.colors[i*3+2]
                    ).getHexString();
              }else{//same color for alll elements
                labelDiv.style.color='#'+color.setRGB(
                      ele.colors[0],
                      ele.colors[1],
                      ele.colors[2]
                    ).getHexString();
              }
              css2obj = new CSS2DObject(labelDiv);
              css2obj.position.set(
                ele.positions[i*3],
                ele.positions[i*3+1],
                ele.positions[i*3+2]);
              css2obj.center.set(0.5,0.5);
              css2obj.layers.set(this.getLayer(ele.tag));
              css2obj.name = ele.label;
              obj.add(css2obj);
            }
            obj.layers.enableAll();
            break;
          case 'octahedron':
            param.radius = ele.radius;
            const octahedrondata = {
              radius: ele.radius
            };
            geometry = new THREE.OctahedronGeometry(
              octahedrondata.radius);
            obj = new THREE.InstancedMesh( geometry, material, len );
            initNewMesh(obj, ele);
            obj.layers.set(this.getLayer(ele.tag));
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function( val) {
              octahedrondata.radius = val;
              updateGroupGeometry(obj, new THREE.OctahedronGeometry(
                octahedrondata.radius));
            });
            break;
          case 'tetrahedron':
            param.radius = ele.radius;
            const tetrahedrondata = {
              radius: ele.radius
            };
            geometry = new THREE.TetrahedronGeometry(
              tetrahedrondata.radius);
            obj = new THREE.InstancedMesh( geometry, material, len );
            initNewMesh(obj, ele);
            obj.layers.set(this.getLayer(ele.tag));
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function( val) {
              tetrahedrondata.radius = val;
              updateGroupGeometry(obj, new THREE.TetrahedronGeometry(
                tetrahedrondata.radius));
            });
            break;
          case 'text':
            param.size = ele.size;
            param.depth = ele.depth;
            const textdata = {
              font: new Font(JSON.parse(ele.font)),
              size: ele.size,
              depth: ele.depth
            }
            geometry = new TextGeometry(ele.label, {
                font: textdata.font,
                size: textdata.size,
                depth: textdata.depth
            });
              geometry.computeBoundingBox();
              const centerOffset = {
                x: - 0.5 * (geometry.boundingBox.max.x - geometry.boundingBox.min.x ),
                y: - 0.5 * (geometry.boundingBox.max.y - geometry.boundingBox.min.y ),
                z: - 0.5 * (geometry.boundingBox.max.z - geometry.boundingBox.min.z )
                };
              obj = new THREE.InstancedMesh( geometry, material, len );
              initNewMesh(obj, ele, centerOffset);
            obj.layers.set(this.getLayer(ele.tag));
              function updateTextGeometry(){
                updateGroupGeometry(obj, new TextGeometry(ele.label, {
                    font: textdata.font,
                    size: textdata.size,
                    depth: textdata.depth
                }))
              }
              folder.add(param, 'size', 0, this.maxRadius).onChange( function(val) {
                textdata.size = val;
                updateTextGeometry()
              });
              folder.add(param, 'depth', 0, this.maxRadius).onChange( function(val) {
                textdata.depth = val;
                updateTextGeometry()
              });
            break;
          case 'torus':
            param.radius = ele.radius;
            param.tube = ele.tube;
            const torusdata = {
              radius: ele.radius,
              tube: ele.tube
            }
            geometry = new THREE.TorusGeometry(
              torusdata.radius,
              torusdata.tube
            )
            obj = new THREE.InstancedMesh( geometry, material, len);
            initNewMesh(obj, ele);
            obj.layers.set(this.getLayer(ele.tag));
            function updateTorusGeometry(){
              updateGroupGeometry(obj, new THREE.TorusGeometry(
                torusdata.radius,
                torusdata.tube
              ))
            }
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function(val) {
              torusdata.radius = val;
              updateTorusGeometry()
            });
            folder.add(param, 'tube', 0, this.maxRadius).onChange( function(val) {
              torusdata.tube = val;
              updateTorusGeometry()
            });
            break;
          default:
        }
        folder.addColor(param, 'color').onChange( function(val){
          param.color = new THREE.Color(val);
          obj.material.color = param.color;
        });
        folder.add(param, 'opacity', 0, 1).onChange( function( val ){
          material.opacity = val;
        });
        folder.add(param, 'transparent').onChange( function( val ){
          material.transparent = val;
        });
        folder.close();
        this.materials.push(material);
        if(ele.layer=='top'){
          if(ele.side=='left'){
              this.objects.add(obj);
          }else{
              this.objects2.add(obj);
          }
        }else{
          if(ele.side=='left'){
              this.objectsBottom.add(obj);
          }else{
              this.objectsBottom2.add(obj);
          }
        }
      }
    }
    
    this.scene.add(this.objects);
    this.sceneBottom.add(this.objectsBottom);
    this.scene2.add(this.objects2);
    this.sceneBottom2.add(this.objectsBottom2);
    for(var key in groupFolderObj){
      groupFolderObj[key].close();
    }
    this.gui.close();
  }
  
  
  resizeCanvas(elmnt){
      var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
      elmnt.onmousedown = dragMouseDown;
      function dragMouseDown(e) {
        e = e || window.event;
        e.preventDefault();
        // get the mouse cursor position at startup:
        pos3 = e.clientX;
        pos4 = e.clientY;
        document.onmouseup = closeDragElement;
        // call a function whenever the cursor moves:
        document.onmousemove = elementDrag;
      }
      var elementDrag =function(e) {
        e = e || window.event;
        e.preventDefault();
        // calculate the new cursor position:
        pos1 = pos3 - e.clientX;
        pos2 = pos4 - e.clientY;
        pos3 = e.clientX;
        pos4 = e.clientY;
        // set the element's new position:
        //elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
        elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";
        this.onWindowResize(this.width-pos1, this.height-pos2);
      }.bind(this)
      var closeDragElement = function() {
        // stop moving when mouse button is released:
        document.onmouseup = null;
        document.onmousemove = null;
      }
  }
  
  onWindowResize(width, height){
    this.width = width;
    this.height = height;
    if(this.sideBySide){
      this.camera.aspect = 2*width/height;
      this.camera2.aspect = 2*width/height;
      this.camera2.updateProjectionMatrix();
      this.labelRenderer.setSize( width/2, height );
      this.labelRenderer2.setSize( width/2, height );
      this.labelRenderer2.domElement.style.top = '-'+2*height+'px';
      this.labelRenderer2.domElement.style.left = width/2+'px';
    }else{
      this.camera.aspect = width / height;
      this.labelRenderer.setSize( width, height );
    }
    this.camera.updateProjectionMatrix();

    this.renderer.setSize( width, height );
    this.labelRenderer.domElement.style.top = '-'+this.height+'px';
    
    this.insetWidth = height / 4; // square
    this.insetHeight = height / 4;
    
    this.cameraInsert.aspect = this.insetWidth / this.insetHeight;
    this.cameraInsert.updateProjectionMatrix();
    
    this.setResizeBlockPos();
  }
  
  animate(inset=true){
    // main scene
    this.renderer.setClearColor( this.background, this.bckalpha );
    
    //controls
    const delta = this.clock.getDelta();
    // animate
    if ( this.animateparam.play ) {
        this.objects.rotation.x += delta * this.animateparam.stepX;
        this.objects.rotation.y += delta * this.animateparam.stepY;
        if(this.sideBySide){
          this.objects2.rotation.x += delta * this.animateparam.stepX;
          this.objects2.rotation.y += delta * this.animateparam.stepY;
        }
        if(this.overlay){
          this.objectsBottom.rotation.x += delta * this.animateparam.stepX;
          this.objectsBottom.rotation.y += delta * this.animateparam.stepY;
          if(this.sideBySide){
            this.objectsBottom2.rotation.x += delta * this.animateparam.stepX;
            this.objectsBottom2.rotation.y += delta * this.animateparam.stepY;
          }
        }
    }
    
    if(this.animateparam.ctrl || this.animateparam.option){
      if (this.animateparam.up) {
          this.objects.rotation.x += delta * this.animateparam.stepX;
          if(this.sideBySide){
            this.objects2.rotation.x += delta * this.animateparam.stepX;
          }
          if(this.overlay){
            this.objectsBottom.rotation.x += delta * this.animateparam.stepX;
            if(this.sideBySide){
              this.objectsBottom2.rotation.x += delta * this.animateparam.stepX;
            }
          }
      }
      if (this.animateparam.down) {
            this.objects.rotation.x -= delta * this.animateparam.stepX;
            if(this.sideBySide){
              this.objects2.rotation.x -= delta * this.animateparam.stepX;
            }
            if(this.overlay){
              this.objectsBottom.rotation.x -= delta * this.animateparam.stepX;
              if(this.sideBySide){
                this.objectsBottom2.rotation.x -= delta * this.animateparam.stepX;
              }
            }
      }
      if (this.animateparam.left) {
            this.objects.rotation.y -= delta * this.animateparam.stepX;
            if(this.sideBySide){
              this.objects2.rotation.y -= delta * this.animateparam.stepX;
            }
            if(this.overlay){
              this.objectsBottom.rotation.y -= delta * this.animateparam.stepX;
              if(this.sideBySide){
                this.objectsBottom2.rotation.y -= delta * this.animateparam.stepX;
              }
            }
      }
      if (this.animateparam.right) {
            this.objects.rotation.y += delta * this.animateparam.stepY;
            if(this.sideBySide){
              this.objects2.rotation.y += delta * this.animateparam.stepY;
            }
            if(this.overlay){
              this.objectsBottom.rotation.y += delta * this.animateparam.stepY;
              if(this.sideBySide){
                this.objectsBottom2.rotation.y += delta * this.animateparam.stepY;
              }
            }
      }
    }else{
      if (this.animateparam.up) {
          this.objects.translateY(this.animateparam.stepY * delta);
          if(this.sideBySide){
            this.objects2.translateY(this.animateparam.stepY * delta);
          }
          if(this.overlay){
            this.objectsBottom.translateY(this.animateparam.stepY * delta);
            if(this.sideBySide){
              this.objectsBottom2.translateY(this.animateparam.stepY * delta);
            }
          }
      }
      if (this.animateparam.down) {
            this.objects.translateY(-this.animateparam.stepY * delta);
            if(this.sideBySide){
              this.objects2.translateY(-this.animateparam.stepY * delta);
            }
            if(this.overlay){
              this.objectsBottom.translateY(-this.animateparam.stepY * delta);
              if(this.sideBySide){
                this.objectsBottom2.translateY(-this.animateparam.stepY * delta);
              }
            }
      }
      if (this.animateparam.left) {
            this.objects.translateX(this.animateparam.stepX * delta);
            if(this.sideBySide){
              this.objects2.translateX(this.animateparam.stepX * delta);
            }
            if(this.overlay){
              this.objectsBottom.translateX(this.animateparam.stepX * delta);
              if(this.sideBySide){
                this.objectsBottom2.translateX(this.animateparam.stepX * delta);
              }
            }
      }
      if (this.animateparam.right) {
            this.objects.translateX(-this.animateparam.stepX * delta);
            if(this.sideBySide){
              this.objects2.translateX(-this.animateparam.stepX * delta);
            }
            if(this.overlay){
              this.objectsBottom.translateX(-this.animateparam.stepX * delta);
              if(this.sideBySide){
                this.objectsBottom2.translateX(-this.animateparam.stepX * delta);
              }
            }
      }
    }
    
    this.controls.update();
    //this.gpuPanel.startQuery();
    if(this.overlay){
      if(this.sideBySide){
        this.controls2.update();
        this.renderer.setScissorTest( true );
        this.renderer.setViewport( 0, 0, this.width/2, this.height );
        this.renderer.setScissor( 0, 0, this.width/2, this.height - this.sliderPos );
        this.renderer.render( this.scene, this.camera );
        this.renderer.setScissor(0, this.height - this.sliderPos, this.width/2, this.height );
        this.renderer.render( this.sceneBottom, this.camera );
        this.renderer.setViewport( this.width/2, 0, this.width/2, this.height );
        this.renderer.setScissor( this.width/2, 0, this.width/2, this.height - this.sliderPos );
        this.renderer.render( this.scene2, this.camera2 );
        this.renderer.setScissor(this.width/2, this.height - this.sliderPos, this.width, this.height );
        this.renderer.render( this.sceneBottom2, this.camera2 );
        this.renderer.setScissorTest( false );
        this.labelRenderer.render(this.scene, this.camera );
        this.labelRenderer.render(this.sceneBottom, this.camera );
        this.labelRenderer2.render(this.scene2, this.camera2 );
        this.labelRenderer2.render(this.sceneBottom2, this.camera2 );
      }else{
        this.renderer.setScissorTest( true );
        this.renderer.setViewport( 0, 0, this.width, this.height );
        this.renderer.setScissor( 0, 0, this.width, this.height - this.sliderPos );
        this.renderer.render( this.scene, this.camera );
        this.renderer.setScissor(0, this.height - this.sliderPos, this.width, this.height );
        this.renderer.render( this.sceneBottom, this.camera );
        this.renderer.setScissorTest( false );
        this.labelRenderer.render(this.scene, this.camera );
        this.labelRenderer.render(this.sceneBottom, this.camera );
      }
    }else{
      if(this.sideBySide){
        this.controls2.update();
        this.renderer.setScissorTest( true );
        this.renderer.setViewport( 0, 0, this.width/2, this.height );
        this.renderer.setScissor( 0, 0, this.width/2, this.height );
        this.renderer.render( this.scene, this.camera );
        this.renderer.setViewport( this.width/2, 0, this.width/2, this.height );
        this.renderer.setScissor(this.width/2, 0, this.width/2, this.height );
        this.renderer.render( this.scene2, this.camera2 );
        this.renderer.setScissorTest( false );
        this.labelRenderer.render(this.scene, this.camera );
        this.labelRenderer2.render(this.scene2, this.camera2 );
      }else{
        this.renderer.setViewport( 0, 0, this.width, this.height );
        this.renderer.render( this.scene, this.camera );
        this.labelRenderer.render(this.scene, this.camera );
        // inset scene
        if(inset){
          this.renderer.setClearColor( 0xffffff, 1 );
          this.renderer.clearDepth(); // important!
          this.renderer.setScissorTest( true );
          this.renderer.setScissor( 20, 20, this.insetWidth, this.insetHeight );
          this.renderer.setViewport( 20, 20, this.insetWidth, this.insetHeight );
          this.cameraInsert.position.copy( this.camera.position );
          this.cameraInsert.quaternion.copy( this.camera.quaternion );
          
          this.scene.background = new THREE.Color(
            Math.max(this.background.r * this.bckalpha - 0.05, 0.01),
            Math.max(this.background.g * this.bckalpha - 0.05, 0.01),
            Math.max(this.background.b * this.bckalpha - 0.05, 0.01)
          );
          this.renderer.render(this.scene, this.cameraInsert );
          // set color back
          this.scene.background = new THREE.Color(
            this.background.r * this.bckalpha,
            this.background.g * this.bckalpha,
            this.background.b * this.bckalpha
          );
          this.renderer.setScissorTest( false );
        }
      }
    }
    //this.gpuPanel.endQuery();
    // stats
    //this.stats.update();
  }
};

export { tjViewer };
