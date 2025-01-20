// please note that the relative folder is related with the version number.
import * as THREE from 'three';
//import Stats from 'three/addons/libs/stats.module.js';
//import { GPUStatsPanel } from 'three/addons/utils/GPUStatsPanel.js';
import { GUI } from 'three/addons/libs/lil-gui.module.min.js';
//import { OrbitControls } from 'three/addons/controls/OrbitControls.js';
import { ArcballControls } from 'three/addons/controls/ArcballControls.js';
// lines
import { Line2 } from 'three/addons/lines/Line2.js';
import { LineMaterial } from 'three/addons/lines/LineMaterial.js';
import { LineGeometry } from 'three/addons/lines/LineGeometry.js';
import { LineSegments2 } from 'three/addons/lines/LineSegments2.js';
import { LineSegmentsGeometry } from 'three/addons/lines/LineSegmentsGeometry.js';
// text
import { Font } from 'three/addons/loaders/FontLoader.js';
import { TextGeometry } from 'three/addons/geometries/TextGeometry.js';
// label
import { CSS2DRenderer, CSS2DObject } from 'three/addons/renderers/CSS2DRenderer.js';
// exporters
import { SVGRenderer } from 'three/addons/renderers/SVGRenderer.js';
import { GLTFExporter } from 'three/addons/exporters/GLTFExporter.js';
import { PLYExporter } from 'three/addons/exporters/PLYExporter.js';
import { STLExporter } from 'three/addons/exporters/STLExporter.js';
import { DRACOExporter } from 'three/addons/exporters/DRACOExporter.js';
// pdf exporters
import {
  Projector,
  RenderableFace,
	RenderableLine,
	RenderableSprite } from "three/addons/renderers/Projector.js";
import { jsPDF } from 'jspdf'; // 2.0.0 version is important! because other version can not load @babel/runtime/helpers/typeof
//pdfRenderer
class PDFRenderer{
  constructor(width, height, backgroundColor){
    this.ambientLight = new THREE.Color(),
    this.directionalLights = new THREE.Color(),
    this.pointLights = new THREE.Color(),
    this.color = new THREE.Color(),
    this.vector3 = new THREE.Vector3();
    this.matrix = new THREE.Matrix4();
    this.vec1 = [];
    this.vec2 = [];
    this.vec3 = [];
    this.lights = [];
    this.widthHalf = width/2;
    this.heightHalf = height/2;
    this.pdf = new jsPDF( (width > height ? 'landscape' : 'portrait'), 'pt', [width, height] );
    this.pdf.setFillColor(backgroundColor.r*255, backgroundColor.g*255, backgroundColor.b*255);
    this.pdf.rect(0, 0, width, height, "F");
    this.projector = new Projector();
    this.sortObjects = true;
    this.sortElements = true;
    this.overdraw = 0.5;
    this.clipRect = new THREE.Box2();
    this.bboxRect = new THREE.Box2();
    this.clipRect.min.set(0, 0);
    this.clipRect.max.set(width, height);
  }

  calculateLights( lights ) {
    this.ambientLight.setRGB( 0, 0, 0 );
    this.directionalLights.setRGB( 0, 0, 0 );
    this.pointLights.setRGB( 0, 0, 0 );

    for ( let l = 0, ll = lights.length; l < ll; l ++ ) {
      const light = lights[ l ];
      const lightColor = light.color;
      if ( light.isAmbientLight ) {
        this.ambientLight.r += lightColor.r;
        this.ambientLight.g += lightColor.g;
        this.ambientLight.b += lightColor.b;
      } else if ( light.isDirectionalLight ) {
        this.directionalLights.r += lightColor.r;
        this.directionalLights.g += lightColor.g;
        this.directionalLights.b += lightColor.b;
      } else if ( light.isPointLight ) {
        this.pointLights.r += lightColor.r;
        this.pointLights.g += lightColor.g;
        this.pointLights.b += lightColor.b;
      }
    }
  }

  calculateLight( lights, position, normal, color ) {
    for ( let l = 0, ll = lights.length; l < ll; l ++ ) {
      const light = lights[ l ];
      const lightColor = light.color;
      if ( light.isDirectionalLight ) {
        const lightPosition = this.vector3.setFromMatrixPosition( light.matrixWorld ).normalize();
        let amount = normal.dot( lightPosition );
        if ( amount <= 0 ) continue;
        amount *= light.intensity;
        color.r += lightColor.r * amount;
        color.g += lightColor.g * amount;
        color.b += lightColor.b * amount;
      } else if ( light.isPointLight ) {
        const lightPosition = this.vector3.setFromMatrixPosition( light.matrixWorld );
        let amount = normal.dot( this.vector3.subVectors( lightPosition, position ).normalize() );
        if ( amount <= 0 ) continue;
        amount *= light.distance == 0 ? 1 : 1 - Math.min( position.distanceTo( lightPosition ) / light.distance, 1 );
        if ( amount == 0 ) continue;
        amount *= light.intensity;
        color.r += lightColor.r * amount;
        color.g += lightColor.g * amount;
        color.b += lightColor.b * amount;
      }
    }
  }

  threeToPdfLineProp ( lineprop ) {
    return lineprop || 0;
  }

  setStyleFromMaterial( material ) {
    this.pdf.setDrawColor( material.color.r*255, material.color.g*255, material.color.b*255 );
    this.pdf.setFillColor( material.color.r*255, material.color.g*255, material.color.b*255 );
    // TODO: material.opacity ??
    if ( material.wireframe ) {
      this.pdf.setLineWidth( material.wireframeLinewidth && !isNaN(material.wireframeLinewidth) ? material.wireframeLinewidth : 1 );
      this.pdf.setLineCap( this.threeToPdfLineProp( material.wireframeLinecap ) );
      this.pdf.setLineJoin( this.threeToPdfLineProp( material.wireframeLinejoin ) );
    } else {
      this.pdf.setLineWidth( material.linewidth && !isNaN(material.linewidth) ? material.linewidth : 1 );
      this.pdf.setLineCap( this.threeToPdfLineProp( material.linecap ) );
      this.pdf.setLineJoin( this.threeToPdfLineProp( material.linejoin ) );
    }
  }
  normalToComponent( normal ) {
    var component = ( normal + 1 ) * 0.5;
    return component < 0 ? 0 : ( component > 1 ? 1 : component );
  }
  setColorForElement( element, material ) {
    if ( material.isMeshBasicMaterial  ) {
      this.color.copy( material.color );
    } else if ( material.isMeshLambertMaterial || material.isMeshPhongMaterial || material.isMeshStandardMaterial ) {
      var diffuse = material.color;
      var emissive = material.emissive;
      var centroid = new THREE.Vector3();
      centroid.copy( element.v1.positionWorld ).add( element.v2.positionWorld ).add( element.v3.positionWorld ).divideScalar( 3 );

      this.color.r = this.ambientLight.r;
      this.color.g = this.ambientLight.g;
      this.color.b = this.ambientLight.b;

      this.calculateLight( this.lights, centroid, element.normalModel, this.color );

      this.color.r = diffuse.r * this.color.r + emissive.r;
      this.color.g = diffuse.g * this.color.g + emissive.g;
      this.color.b = diffuse.b * this.color.b + emissive.b;
    } else if ( material instanceof THREE.MeshDepthMaterial ) {
      var w = 1 - ( material.__2near / (material.__farPlusNear - element.z * material.__farMinusNear) );
      this.color.setRGB( w, w, w );
    } else if ( material.isMeshNormalMaterial ) {
      this.color.setRGB( this.normalToComponent( element.normalModel.x ),
                     this.normalToComponent( element.normalModel.y ),
                     this.normalToComponent( element.normalModel.z ) );

    }

    this.pdf.setDrawColor( this.color.r*255, this.color.g*255, this.color.b*255 );
    this.pdf.setFillColor( this.color.r*255, this.color.g*255, this.color.b*255 );
  }
  positionScreenToPage( position ) {
    position.x *= this.widthHalf;
    position.x += this.widthHalf;
    position.y *= -this.heightHalf;
    position.y += this.heightHalf;
  }
  renderSprite( v1, element, material ) {
    if ( material instanceof THREE.LineBasicMaterial ) {
      this.setStyleFromMaterial( material );
    }
    this.pdf.lines( [[1, 1]],
                    v1.positionScreen.x, v1.positionScreen.y,
                    [1,1], 'S' );
  }
  renderLine ( v1, v2, element, material, scene ) {
    if ( material instanceof THREE.LineBasicMaterial ) {
      this.setStyleFromMaterial( material );
    }
    this.pdf.lines( [[v2.positionScreen.x-v1.positionScreen.x,
                      v2.positionScreen.y-v1.positionScreen.y]],
                    v1.positionScreen.x, v1.positionScreen.y,
                    [1,1],
                    'S' );
  }
  renderFace3 ( v1, v2, v3, element, material, scene ) {
    //console.log('renderFace3');
    this.setStyleFromMaterial( material );
    this.setColorForElement( element, material );
    //console.log(material);
    //console.log(this.pdf.getFillColor());
    //console.log(this.pdf.getDrawColor());
    this.pdf.triangle( v1.positionScreen.x, v1.positionScreen.y,
                       v2.positionScreen.x, v2.positionScreen.y,
                       v3.positionScreen.x, v3.positionScreen.y,
                       material.wireframe ? 'S' : 'F' );// Stroke or Fill, FD: fill then stroke
  }
  renderLine2(v1, v2, material){
      this.setStyleFromMaterial( material );
      this.pdf.setDrawColor( this.color.r*255, this.color.g*255, this.color.b*255 );
      this.pdf.setFillColor( this.color.r*255, this.color.g*255, this.color.b*255 );
      //console.log(this.color);
      this.pdf.lines( [[v2.x-v1.x,
                        v2.y-v1.y]],
                      v1.x, v1.y,
                      [1,1],
                      'S' );
  }
  renderTriangle ( v1, v2, v3, element, material ) {
    //this.setStyleFromMaterial( material );
    //this.setColorForElement( element, material );
    //console.log(material);
    //console.log(this.pdf.getFillColor());
    //console.log(this.pdf.getDrawColor());
    this.pdf.triangle( v1.x, v1.y,
                       v2.x, v2.y,
                       v3.x, v3.y,
                       'F' );// Stroke or Fill, FD: fill then stroke
  }
  renderLabel(v3, label, color){
    this.pdf.setTextColor( color );
    //console.log(this.pdf.getTextColor());
    this.pdf.text(label, v3.x, v3.y);
  }

  render(scene, camera){
    console.log(scene);
    var renderData = this.projector.projectScene(scene, camera, this.sortObjects, this.sortElements );
    this.lights = renderData.lights;
    this.calculateLights(this.lights);

    for( let e = 0, el = renderData.elements.length; e<el; e++){
      const element = renderData.elements[ e ];
      const material = element.material;
      //console.log(element);
      if ( material === undefined || material.opacity === 0 ) continue;
      if ( element instanceof RenderableSprite ) {
        this.vec1 = element;
        this.positionScreenToPage( this.vec1.positionScreen );
        this.renderSprite( this.vec1, element, material );
      } else if ( element instanceof RenderableLine ) {
        this.vec1 = element.v1; this.vec2 = element.v2;
        this.positionScreenToPage( this.vec1.positionScreen );
        this.positionScreenToPage( this.vec2.positionScreen );
        this.bboxRect.setFromPoints( [new THREE.Vector2(this.vec1.positionScreen.x, this.vec1.positionScreen.y),
                                     new THREE.Vector2(this.vec2.positionScreen.x, this.vec2.positionScreen.y)] );
        if ( !this.clipRect.intersectsBox( this.bboxRect ) ) {
          continue;
        }
        this.renderLine( this.vec1, this.vec2, element, material, scene );
      } else if ( element instanceof RenderableFace ) {
        this.vec1 = element.v1; this.vec2 = element.v2; this.vec3 = element.v3;
        //console.log(this.vec1.positionScreen);
        //console.log(this.vec2.positionScreen);
        //console.log(this.vec3.positionScreen);
        if ( this.vec1.positionScreen.z < - 1 || this.vec1.positionScreen.z > 1 ) continue;
        if ( this.vec2.positionScreen.z < - 1 || this.vec2.positionScreen.z > 1 ) continue;
        if ( this.vec3.positionScreen.z < - 1 || this.vec3.positionScreen.z > 1 ) continue;

        this.positionScreenToPage( this.vec1.positionScreen );
        this.positionScreenToPage( this.vec2.positionScreen );
        this.positionScreenToPage( this.vec3.positionScreen );

        this.bboxRect.setFromPoints( [new THREE.Vector2(this.vec1.positionScreen.x, this.vec1.positionScreen.y),
                                     new THREE.Vector2(this.vec2.positionScreen.x, this.vec2.positionScreen.y),
                                     new THREE.Vector2(this.vec3.positionScreen.x, this.vec3.positionScreen.y)]);
        //console.log('RenderableFace');
        //console.log(this.clipRect);
        //console.log(this.bboxRect);
        if ( !this.clipRect.intersectsBox( this.bboxRect ) ) {
          continue;
        }
        this.renderFace3( this.vec1, this.vec2, this.vec3, element, material, scene );
      }
    }
    scene.traverse(obj =>  {
      //CSS2DObject
      if(obj.isCSS2DObject === true){
        if(obj.position != undefined && obj.visible && camera.layers.test(obj.layers)){
          this.vector3.copy(obj.position);
          this.vector3.project(camera);
          //console.log(this.vector3);
          //console.log(obj.position);
          this.positionScreenToPage( this.vector3 );
          this.renderLabel( this.vector3, obj.name, obj.element.style.color );
        }
      }
      //line2 or LineSegments2object
      if(obj.isLine2 === true || obj.isLineSegments2 === true && obj.material.visible && camera.layers.test(obj.layers)){
        //console.log(obj);
        if(obj.geometry.attributes != undefined && obj.visible){
          var start=obj.geometry.attributes.instanceStart;
          var color = obj.geometry.attributes.instanceColorStart.data.array;
          for(var i=0; i<start.data.count; i++){
            var k=i*start.data.stride;
            const vec1 = new THREE.Vector3();
            const vec2 = new THREE.Vector3();
            vec1.x=start.data.array[k];
            vec1.y=start.data.array[k+1];
            vec1.z=start.data.array[k+2];
            vec2.x=start.data.array[k+3];
            vec2.y=start.data.array[k+4];
            vec2.z=start.data.array[k+5];
            vec1.project(camera);
            vec2.project(camera);
            this.positionScreenToPage( vec1 );
            this.positionScreenToPage( vec2 );

            this.bboxRect.setFromPoints( [new THREE.Vector2(vec1.x, vec1.y),
                                         new THREE.Vector2(vec2.x, vec2.y)] );
            if ( !this.clipRect.intersectsBox( this.bboxRect ) ) {
              continue;
            }
            this.color.setRGB( color[k+0], color[k+1], color[k+2] );
            this.renderLine2( vec1, vec2, obj.material );
          }
        }
      }
      /*if(obj.isInstancedMesh === true && obj.material.visible && camera.layers.test(obj.layers)){
        console.log(obj);
        var position = obj.geometry.attributes.position;
        var normal = obj.geometry.attributes.normal;
        console.log(position);
        var itemSize = position.itemSize;
        var index = obj.geometry.index;
        console.log(index);
        if(itemSize==3 && index){
          for(var w=0; w<obj.count; w++){
            const color = new THREE.Color();
            obj.getColorAt(w, this.color);
            if(this.color.r && this.color.g && this.color.b){
              this.pdf.setDrawColor( this.color.r*255, this.color.g*255, this.color.b*255 );
              this.pdf.setFillColor( this.color.r*255, this.color.g*255, this.color.b*255 );
            }
            obj.getMatrixAt(w, this.matrix);
            for(var i=0; i<index.count/itemSize; i++){
              var k=i*itemSize;
              const vec1 = new THREE.Vector3();
              const vec2 = new THREE.Vector3();
              const vec3 = new THREE.Vector3();
              vec1.x=position.array[index.array[k]];
              vec1.y=position.array[index.array[k]+1];
              vec1.z=position.array[index.array[k]+2];
              vec2.x=position.array[index.array[k+1]];
              vec2.y=position.array[index.array[k+1]+1];
              vec2.z=position.array[index.array[k+1]+2];
              vec3.x=position.array[index.array[k+2]];
              vec3.y=position.array[index.array[k+2]+1];
              vec3.z=position.array[index.array[k+2]+2];
              vec1.applyMatrix4( obj.matrixWorld ).applyMatrix4(this.matrix);
              vec2.applyMatrix4( obj.matrixWorld ).applyMatrix4(this.matrix);
              vec3.applyMatrix4( obj.matrixWorld ).applyMatrix4(this.matrix);
              vec1.project(camera);
              vec2.project(camera);
              vec3.project(camera);
              this.positionScreenToPage( vec1 );
              this.positionScreenToPage( vec2 );
              this.positionScreenToPage( vec3 );
  
              this.bboxRect.setFromPoints( [new THREE.Vector2(vec1.x, vec1.y),
                                           new THREE.Vector2(vec2.x, vec2.y),
                                           new THREE.Vector2(vec3.x, vec3.y)] );
              if ( !this.clipRect.intersectsBox( this.bboxRect ) ) {
                continue;
              }
              this.renderTriangle(vec1, vec2, vec3, obj, obj.material);
            }
          }
        }
      }*/
    });
    //console.log(this.pdf);
  }
};


// module Widget
class tjViewer{
  constructor(el, width, height){
    // clear el
    el.textContent = '';
    //add gui first
    this.gui = new GUI({container:el});
    this.dragGUI(this.gui.domElement);
    
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
    
    // top & bottom slider
    this.slider = document.createElement('div');
    this.slider.className = 'tjviewer_slider';
    this.slider.style.display = 'none';
    this.container = el;
    el.appendChild(this.slider);
    this.sliderPos = this.height/2;
    
    // resize canvas blocker
    this.resizeBlock = document.createElement('div');
    this.resizeBlock.className = 'tjviewer_resizeblock';
    this.setResizeBlockPos();
    el.appendChild(this.resizeBlock);
    this.resizeCanvas(this.resizeBlock);
    
    // title region
    this.titleBox = document.createElement('div');
    this.titleBox.className = 'tjviewer_titlebox';
    el.appendChild(this.titleBox);
    this.titleBox2 = document.createElement('div');
    this.titleBox2.className = 'tjviewer_titlebox2';
    el.appendChild(this.titleBox2);
      
    this.perspectiveDistance = -10;
    this.orthographicDistance = 120;
    this.fov = 50;
    this.near = .0001;
    this.far = 1000;
    this.camera = this.makePerspectiveCamera();
    this.camera.position.set( 0, 0, this.perspectiveDistance ); // set to x, y, z;
    this.camera2 = this.makePerspectiveCamera();
    this.camera2.position.set( 0, 0, this.perspectiveDistance ); // set to x, y, z;
    // link zoom in and out
    this.linkPan();
    
    // viewport
    this.cameraInsert = new THREE.PerspectiveCamera( this.fov, 1, this.near, this.far );
    this.cameraInsert.position.copy( this.camera.position );
    this.cameraInsert.layers.enableAll();
    
    // mouse controls
    this.controls = this.makeControls(this.camera, this.labelRenderer, this.scene);
    this.controls2 = this.makeControls( this.camera2, this.labelRenderer2, this.scene2 );
    
    /*
    this.stats = new Stats();
    el.appendChild(this.stats.dom);
    
    this.gpuPanel = new GPUStatsPanel( this.renderer.getContext() );
    this.stats.addPanel( this.gpuPanel );
    this.stats.showPanel( 0 );*/
    
    this.clock = new THREE.Clock();
    this.insetCamera = true;
    this.maxRadius = 1;
    this.maxLineWidth = 50;
    this.layer = {};
    this.symbols = [];//save all gene symbols
    
    // add GUIs
    this.setGUI();
  }
  
  dragGUI(elmnt){
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
  
  setCameraGUI(){
    this.cameraparam = {
      YX_aspect : this.camera.aspect,
      type : 'Perspective',
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
      insetCamera: true,
      setAspect : function(){
        this.camera.aspect = this.cameraparam.YX_aspect;
        this.camera2.aspect = this.cameraparam.YX_aspect;
        this.camera.updateProjectionMatrix();
        this.camera2.updateProjectionMatrix();
      }.bind(this),
      setPosition : function(){
        this.objects.position.set(
          this.cameraparam.center_x,
          this.cameraparam.center_y,
          this.cameraparam.center_z);
        if(this.overlay){
          this.objectsBottom.position.set(
            this.cameraparam.center_x,
            this.cameraparam.center_y,
            this.cameraparam.center_z);
        }
        if(this.sideBySide){
          this.objects2.position.set(
            this.cameraparam.center_x,
            this.cameraparam.center_y,
            this.cameraparam.center_z);
          if(this.overlay){
            this.objectsBottom2.position.set(
              this.cameraparam.center_x,
              this.cameraparam.center_y,
              this.cameraparam.center_z);
          }
        }
      }.bind(this)
    }
    const cameraGUI = this.gui.addFolder('camera');
    cameraGUI.add(this.cameraparam, 'YX_aspect', 0, 10).onChange( function(val){
      this.cameraparam.YX_aspect = val;
      this.cameraparam.changed = true;
    }.bind(this)).onFinishChange(this.cameraparam.setAspect);
    cameraGUI.add(this.cameraparam, 'type', [ 'Orthographic', 'Perspective' ] )
      .name( 'projection method' ).onChange( function () {
        this.removeLinkedControls();
        this.camera = this.setCamera();
        this.controls.setCamera(this.camera);
        this.camera2 = this.setCamera();
        this.controls2.setCamera(this.camera2);
        this.linkPan();
        this.linkControls();
        this.animate();
      }.bind(this) );
    cameraGUI.add(this.cameraparam, 'center_x', -10, 10).onChange( function(val){
      this.cameraparam.center_x = val;
      this.cameraparam.setPosition();
    }.bind(this));
    cameraGUI.add(this.cameraparam, 'center_y', -10, 10).onChange( function(val){
      this.cameraparam.center_y = val;
      this.cameraparam.setPosition();
    }.bind(this));
    cameraGUI.add(this.cameraparam, 'center_z', -10, 10).onChange( function(val){
      this.cameraparam.center_z = val;
      this.cameraparam.setPosition;
    }.bind(this));
    cameraGUI.add(this.cameraparam, 'world_center');
    cameraGUI.add(this.cameraparam, 'object_center');
    cameraGUI.add(this.cameraparam, 'insetCamera').onChange( function(val){
      this.cameraparam.insetCamera = val;
      this.insetCamera = val;
    }.bind(this));
    cameraGUI.close();
  }
  
  setAutocompleteDatalist(){
    if(document.getElementById('symbollist')==null){
      var a = document.createElement('datalist');
      a.setAttribute('id', 'symbollist');
      for(var i=0; i<this.symbols.length; i++){
        var b = document.createElement('option');
        b.setAttribute('value', this.symbols[i]);
        b.setAttribute('label', this.symbols[i]);
        a.appendChild(b);
      }
      this.container.appendChild(a);
    }
    
    return('symbollist');
  }
  
  searchGeneByGeneName(keyword, scene, sceneBottom){
    let result = [];
    scene.traverse(obj =>  {
      if(obj.isCSS2DObject === true){
        if(obj.name.toUpperCase()==keyword.toUpperCase()){
          result.push(obj);
        }
      }
    });
    sceneBottom.traverse(obj =>  {
      if(obj.isCSS2DObject === true){
        if(obj.name.toUpperCase()==keyword.toUpperCase()){
          result.push(obj);
        }
      }
    });
    return(result);
  }
  searchGeneBodyByGeneName(keyword, scene, sceneBottom){
    let gene_body = [];
    scene.traverse(obj =>  {
      if(obj.isLine2 === true){
        if(obj.name.toUpperCase()==keyword.toUpperCase()){
          gene_body.push(obj);
        }
      }
    });
    sceneBottom.traverse(obj =>  {
      if(obj.isLine2 === true){
        if(obj.name.toUpperCase()==keyword.toUpperCase()){
          gene_body.push(obj);
        }
      }
    });
    return(gene_body);
  }
  setSearchGUI(){
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
          let gene_body = this.searchGeneBodyByGeneName(searchparam.keyword, this.scene, this.sceneBottom);
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
            retult = this.searchGeneByGeneName(searchparam.keyword, this.scene, this.sceneBottom);
            if(result.length>0){
              const pos = result[0].position;
              this.camera.position.set( pos.x, pos.y, pos.z );
              this.animate();
            }
          }
          if(this.sideBySide){
            result = [];
            gene_body = this.searchGeneBodyByGeneName(searchparam.keyword, this.scene2, this.sceneBottom2);
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
              retult = this.searchGeneByGeneName(searchparam.keyword, this.scene2, this.sceneBottom2);
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
    const keyword=searchGUI.add(searchparam, 'keyword').onChange(function(val){
      searchparam.keyword = val;
    }).onFinishChange(searchparam.search);
    keyword.$input.setAttribute("list", 'symbollist');
    keyword.$input.setAttribute("autocomplete", 'off');
    keyword.$input.setAttribute('size', 10);
    searchGUI.add(searchparam, 'search');
  }
  
  setAnimatGUI(){
    this.animateparam = {
      play : false,
      stepX : 0.3,
      stepY : 0.3,
      stepZ : 0.3, 
      linked: true,
      up : false,
      down : false,
      left : false,
      right : false,
      ctrl : false,
      option : false,
      'scene' : 'left',
      'rotate x' : 0,
      'rotate y' : 0,
      'rotate z' : 0,
      'flip' : ''
    };
    
    const animateGUI = this.gui.addFolder('animate');
    animateGUI.add(this.animateparam, 'play');
    animateGUI.add(this.animateparam, 'stepX', 0, 5 ).onChange( function ( val ) {
      this.animateparam.stepX = val;
    }.bind(this) );
    animateGUI.add(this.animateparam, 'stepY', 0, 5 ).onChange( function ( val ) {
      this.animateparam.stepY = val;
    }.bind(this) );
    
    const rotationGUI = this.gui.addFolder('rotation');
    rotationGUI.add(this.animateparam, 'scene', ['left', 'right']).onChange( function( val) {
              this.animateparam.scene = val;
    }.bind(this));
    rotationGUI.add(this.animateparam, 'rotate x', -2*Math.PI, 2*Math.PI).onChange( function( val) {
      this.rotateXYZ('x', val);
    }.bind(this));
    rotationGUI.add(this.animateparam, 'rotate y', -2*Math.PI, 2*Math.PI).onChange( function( val) {
      this.rotateXYZ('y', val);
    }.bind(this));
    rotationGUI.add(this.animateparam, 'rotate z', -2*Math.PI, 2*Math.PI).onChange( function( val) {
      this.rotateXYZ('z', val);
    }.bind(this));
    rotationGUI.add(this.animateparam, 'flip', ['', 'x', 'y', 'z']).onChange(this.flipXYZ.bind(this));
    this.animateLinkedGUI = rotationGUI.add(this.animateparam, 'linked').onChange( function(val){
      this.animateparam.linked = val;
    }.bind(this)).hide();
    rotationGUI.close();
    
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
    this.linkControls();
    this.addGizmos();
    
    animateGUI.close();
  }
  
  setExporterGUI(){
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
      width: this.width,
      height: this.height,
      export : function() {
        let exporter;
        var oldWidth,oldHeight;
        switch(expparam.format){
          case 'png':
            oldWidth = this.width;
            oldHeight = this.height;
            if(oldWidth!=expparam.width || oldHeight!=expparam.height){
              this.onWindowResize(expparam.width, expparam.height);
            }
            this.animate();
            this.renderer.domElement.toBlob(blob =>{
              saveBlob(blob, expparam.filename+'.'+expparam.format);
            });
            if(oldWidth!=expparam.width || oldHeight!=expparam.height){
              this.onWindowResize(oldWidth, oldHeight);
            }
            break;
          case 'pdf':
            oldWidth = this.width;
            oldHeight = this.height;
            if(oldWidth!=expparam.width || oldHeight!=expparam.height){
                this.onWindowResize(expparam.width, expparam.height);
            }
            if(this.sideBySide){
              var pdfRenderer = new PDFRenderer(expparam.width/2, expparam.height, this.background);
              pdfRenderer.render(this.scene, this.camera);
              pdfRenderer.pdf.save(expparam.filename+'.left.'+expparam.format);
              var pdfRenderer2 = new PDFRenderer(expparam.width/2, expparam.height, this.background2);
              pdfRenderer2.render(this.scene2, this.camera2);
              pdfRenderer2.pdf.save(expparam.filename+'.right.'+expparam.format);
            }else{
              var pdfRenderer = new PDFRenderer(expparam.width, expparam.height, this.background);
              pdfRenderer.render(this.scene, this.camera);
              pdfRenderer.pdf.save(expparam.filename+'.'+expparam.format);
            }
            if(this.overlay){
              if(this.sideBySide){
                var pdfRenderer = new PDFRenderer(this.width/2, this.height, this.backgroundBottom);
                pdfRenderer.render(this.sceneBottom, this.camera);
                pdfRenderer.pdf.save(expparam.filename+'.leftBottom.'+expparam.format);
                var pdfRenderer2 = new PDFRenderer(this.width/2, this.height, this.backgroundBottom2);
                pdfRenderer2.render(this.sceneBottom2, this.camera2);
                pdfRenderer2.pdf.save(expparam.filename+'.rightBottom.'+expparam.format);
              }else{
                var pdfRenderer = new PDFRenderer(this.width, this.height, this.backgroundBottom);
                pdfRenderer.render(this.sceneBottom, this.camera);
                pdfRenderer.pdf.save(expparam.filename+'.bottom.'+expparam.format);
              }
            }
            if(oldWidth!=expparam.width || oldHeight!=expparam.height){
              this.onWindowResize(oldWidth, oldHeight);
            }
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
              oldWidth = this.width;
              oldHeight = this.height;
              if(oldWidth!=expparam.width || oldHeight!=expparam.height){
                this.onWindowResize(expparam.width, expparam.height);
              }
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
                if(oldWidth!=expparam.width || oldHeight!=expparam.height){
                  this.onWindowResize(oldWidth, oldHeight);
                }
              }.bind(this);
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
    this.exporterGUI = this.gui.addFolder('exporter');
    this.exporterGUI.add(expparam, 'filename').onChange(
      val => expparam.filename = val
    );
    var availableFormat = ['drc', 'gltf', 'pdf', 'ply', 'png', 'stl', 'svg', 'video'];
    var supportFormat = ['png', 'pdf', 'video'];
    var exporterDuration = this.exporterGUI.add(expparam, 'duration', 0, 120, 1).onChange(
      val => expparam.duration = val
    ).hide();
    this.exporterGUI.add(expparam, 'format', supportFormat).onChange(
      val => {
        expparam.format = val;
        if(val=='video'){
          exporterDuration.show();
        }else{
          exporterDuration.hide();
        }
      }
    );
    this.exporterGUIwidth =  this.exporterGUI.add(expparam, 'width', 0, 15000).onChange(
     val => expparam.width = val
    );
    this.exporterGUIheight = this.exporterGUI.add(expparam, 'height', 0, 15000).onChange(
     val => expparam.height = val
    );
    const exporterBotton = this.exporterGUI.add(expparam, 'export');
    this.exporterGUI.close();
  }
  setExporterGUIpara(){
    this.exporterGUIwidth.setValue(this.width);
    this.exporterGUIheight.setValue(this.height);
  }
  
  setBackgroundColorGUI(){
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
  }
  
  setLightGUI(){
    //soft white light
    const ambientLight = new THREE.AmbientLight( 0x404040, 2);
    this.scene.add( ambientLight );
    const ambientparams = {
      AmbientColor: ambientLight.color.getHex(),
      AmbientIntensity: ambientLight.intensity
    };
    
    // spotlight GUI
    let directionalLight1,directionalLight2; 
    directionalLight1 = new THREE.DirectionalLight( 0xffffff, 5 );
    directionalLight1.position.set( 2.5, 5, -25 );
    directionalLight2 = new THREE.DirectionalLight( 0x606060, 2 );
    directionalLight2.position.set( -2.5, 25, 5 );
    this.scene.add( directionalLight1 );
    this.scene.add( directionalLight2 );
    
    const lightparams = {
      color: directionalLight1.color.getHex(),
      intensity: directionalLight1.intensity,
      x: 2.5,
      y: 5,
      z: -25,
      'auxiliary color': directionalLight2.color.getHex(),
      'auxiliary intensity': directionalLight2.intensity,
      'auxiliary x': -2.5,
      'auxiliary y': 25,
      'auxiliary z': 5,
      setPosition: function(main){
        if(main){
          directionalLight1.position.set(lightparams.x, lightparams.y, lightparams.z);
        }else{
          directionalLight2.position.set(lightparams['auxiliary x'], lightparams['auxiliary y'], lightparams['auxiliary z']);
        }
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
      directionalLight1.color.setHex( val );
    } );
    spotlightGUI.add( lightparams, 'intensity', 0, 100 ).onChange( function ( val ) {
      directionalLight1.intensity = val;
    } );
    spotlightGUI.add( lightparams, 'x', -50, 50 ).onChange( function ( val ) {
      lightparams.x = val;
      lightparams.setPosition(true);
    } );
    spotlightGUI.add( lightparams, 'y', -50, 50 ).onChange( function ( val ) {
      lightparams.y = val;
      lightparams.setPosition(true);
    } );
    spotlightGUI.add( lightparams, 'z', -50, 50 ).onChange( function ( val ) {
      lightparams.z = val;
      lightparams.setPosition(true);
    } );
    spotlightGUI.addColor( lightparams, 'auxiliary color' ).onChange( function ( val ) {
      directionalLight2.color.setHex( val );
    } );
    spotlightGUI.add( lightparams, 'auxiliary intensity', 0, 50 ).onChange( function ( val ) {
      directionalLight2.intensity = val;
    } );
    spotlightGUI.add( lightparams, 'auxiliary x', -50, 50 ).onChange( function ( val ) {
      lightparams['auxiliary x'] = val;
      lightparams.setPosition(false);
    } );
    spotlightGUI.add( lightparams, 'auxiliary y', -50, 50 ).onChange( function ( val ) {
      lightparams['auxiliary y'] = val;
      lightparams.setPosition(false);
    } );
    spotlightGUI.add( lightparams, 'auxiliary z', -50, 50 ).onChange( function ( val ) {
      lightparams['auxiliary z'] = val;
      lightparams.setPosition(false);
    } );
    spotlightGUI.close();
  }
  
  setMeasureGUI(){
    const measureGUI = this.gui.addFolder('measure TSS distance');
    const measureparam = {
      'measure by cursor': false,
      'gene 1': '',
      'gene 2': '',
      'result' : "0",
      'clear' : function(){
                  markerA.visible = false;
                  markerB.visible = false;
                  markerA2.visible = false;
                  markerB2.visible = false;
                  measureparam['gene 1'] = '';
                  measureparam['gene 2'] = '';
                  measureparam.result = "0";
                  labelDiv.textContent = '';
                  labelDiv2.textContent = '';
                  setLine(line, result, new THREE.Vector3(), new THREE.Vector3());
                  setLine(line2, result2, new THREE.Vector3(), new THREE.Vector3());
                }
    };
    /*measureGUI.add(measureparam, 'measure by cursor').onChange((val)=>{
      if(val){
        document.addEventListener("mousedown", startMeasure, false);
      }else{
        endMeasure();
      }
    });*/
    const g1=measureGUI.add(measureparam, 'gene 1').onChange(val => {starMeasureByGene(val)});
    const g2=measureGUI.add(measureparam, 'gene 2').onChange(val => {starMeasureByGene(val)});
    g1.$input.setAttribute("list", 'symbollist');
    g1.$input.setAttribute("autocomplete", 'off');
    g1.$input.setAttribute('size', 10);
    g2.$input.setAttribute("list", 'symbollist');
    g2.$input.setAttribute("autocomplete", 'off');
    g2.$input.setAttribute("size", 10);
    const distancePlace = measureGUI.add(measureparam, 'result');
    measureGUI.add(measureparam, 'clear');
    
    var points = [
        new THREE.Vector3(),
        new THREE.Vector3()
    ]
    var points2 = [
        new THREE.Vector3(),
        new THREE.Vector3()
    ]
    var clicks = 0;
    var clicks2 = 0;
    var vector = new THREE.Vector2();
    var raycaster = new THREE.Raycaster();
        
    var markerA = new THREE.Mesh(
        new THREE.SphereGeometry(0.1, 10, 20),
        new THREE.MeshBasicMaterial({
          color: 0x00ff55
        })
    );
    markerA.visible = false;
    var markerB = new THREE.Mesh(
        new THREE.SphereGeometry(0.1, 10, 20),
        new THREE.MeshBasicMaterial({
          color: 0xff5555
        })
    );
    markerB.visible = false;
    var markers = [
        markerA, markerB
    ];
    var markerA2 = markerA.clone();
    var markerB2 = markerB.clone();
    var markers2 = [
        markerA2, markerB2
    ];
    
    var lineGeometry = new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(), new THREE.Vector3()]);
    var lineGeometry2 = new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(), new THREE.Vector3()]);
    var lineMaterial = new THREE.LineBasicMaterial({
        color: 0xff5555
      });
    var line = new THREE.Line(lineGeometry, lineMaterial);
    var line2 = new THREE.Line(lineGeometry2, lineMaterial);
    
    var labelDiv = document.createElement('div');
    labelDiv.style.backgroundColor = 'transparent';
    labelDiv.style.color='#ff5555';
    labelDiv.textContent = '';
    var labelDiv2 = document.createElement('div');
    labelDiv2.style.backgroundColor = 'transparent';
    labelDiv2.style.color='#ff5555';
    labelDiv2.textContent = '';
    var result = new CSS2DObject(labelDiv);
    var result2 = new CSS2DObject(labelDiv2);
    
    var getCurrentCursorPos = function(event) {
      vector.x = ( (event.clientX - this.container.offsetLeft) / this.width ) * 2 - 1;
      vector.y = - ( (event.clientY - this.container.offsetTop) / this.height ) * 2 + 1;
    }.bind(this);

    var getIntersections = function(event, scene, camera) {
        getCurrentCursorPos(event);
        raycaster.setFromCamera(vector, camera);
        var target = scene.children;
        // remove unwanted target
        target = target.filter(v => !v.isLight && !v.isMesh && !v.isLine && v.children.length!=3);
        // not work just because all the objects are grouped in transformed Coordinates.
        // need to write a intersects function
        var intersects = raycaster.intersectObjects(target);
        //console.log(intersects);
        return intersects;
    };
    
    function setLine(l, res, vectorA, vectorB) {
        l.geometry.attributes.position.setXYZ(0, vectorA.x, vectorA.y, vectorA.z);
        l.geometry.attributes.position.setXYZ(1, vectorB.x, vectorB.y, vectorB.z);
        l.geometry.attributes.position.needsUpdate = true;
        
        res.position.set(
          (vectorA.x + vectorB.x)/2,
          (vectorA.y + vectorB.y)/2,
          (vectorA.z + vectorB.z)/2
        );
        res.center.set(0.5,0.5);
    }
    
    function showDistance(line, result, points, labelDiv, label2=''){
      var distance = points[0].distanceTo(points[1]);
      let formattedNumber = distance.toLocaleString('en-US', {
              minimumIntegerDigits: 1,
              useGrouping: false
      })
      measureparam.result = formattedNumber;
      distancePlace.setValue(formattedNumber+label2);
      labelDiv.textContent = formattedNumber;
      setLine(line, result, points[0], points[1]);
    }
    
    function swapPosition(markers, points){
      var position = markers[1].position.clone();
      markers[1].position.copy(markers[0].position);
      markers[0].position.copy(position);
      points[1].copy(points[0]);
      points[0].copy(position);
    }
    
    var more = false;
    function showResults1(collection, scene){
      if (collection.length > 0) {
          points[clicks].copy(collection[0].point);
          markers[clicks].position.copy(collection[0].point);
          setLine(line, result, collection[0].point, collection[0].point);
          clicks++;
          if (clicks > 1){
            markerB.visible = true;
            showDistance(line, result, points, labelDiv);
            clicks = 0;
            more = true;
          }else{
            if(clicks == 1){
              if(more){
                swapPosition(markers, points);
                showDistance(line, result, points, labelDiv);
              }else{
                markerA.visible = true;
                markerB.visible = false;
              }
            }
          }
      }
    }
    var more2 = false;
    function showResults2(collection, scene){
      if (collection.length > 0) {
          points2[clicks2].copy(collection[0].point);
          markers2[clicks2].position.copy(collection[0].point);
          setLine(line2, result2, collection[0].point, collection[0].point);
          clicks2++;
          if (clicks2 > 1){
            markerB2.visible = true;
            showDistance(line2, result2, points2, labelDiv2, '; '+distancePlace.getValue());
            clicks2 = 0;
            more2 = true;
          }else{
            if(clicks2 == 1){
              if(more){
                swapPosition(markers2, points2);
                showDistance(line2, result2, points2, labelDiv2, '; '+distancePlace.getValue());
              }else{
                markerA2.visible = true;
                markerB2.visible = false;
              }
            }
          }
      }
    }
    
    const markerGroup = new THREE.Group();
    markerGroup.add(markerA);
    markerGroup.add(markerB);
    markerGroup.add(line);
    markerGroup.add(result);
    //this.objects.add(markerGroup);//this broken the rotation, don't know why
    this.objects.add(markerA);
    this.objects.add(markerB);
    this.objects.add(line);
    this.objects.add(result);
    this.scene.add(markerGroup);
    const markerGroup2 = new THREE.Group();
    markerGroup2.add(markerA2);
    markerGroup2.add(markerB2);
    markerGroup2.add(line2);
    markerGroup2.add(result2);
    //this.objects2.add(markerGroup2);
    this.objects2.add(markerA2);
    this.objects2.add(markerB2);
    this.objects2.add(line2);
    this.objects2.add(result2);
    this.scene2.add(markerGroup2);
    var startMeasure = function(event){
      event.preventDefault();
      
      var intersects = getIntersections(event, this.scene, this.camera);
      if(intersects.length>0){
        showResults1(intersects, this.scene);
      }
      if(this.sideBySide){
        var intersects2 = getIntersections(event, this.scene2, this.camera2);
        if(intersects2.length>0){
          showResults2(intersects2, this.scene2);
        }
      }
    }.bind(this);
    
    var endMeasure = function(){
      document.removeEventListener("mousedown", startMeasure);
      markerA.visible = false;
      markerB.visible = false;
      markerA2.visible = false;
      markerB2.visible = false;
    }.bind(this);
    
    var checkGene = function(val){
      var gene_body = this.searchGeneByGeneName(val, this.scene, this.sceneBottom);
      if(gene_body.length>0){
        var wpos = new THREE.Vector3();
        gene_body[0].getWorldPosition(wpos);
        var intersects = [{point:wpos}];
        showResults1(intersects, this.scene);
      }
      var gene_body2 = this.searchGeneByGeneName(val, this.scene2, this.sceneBottom2);
      if(gene_body2.length>0){
        var wpos2 = new THREE.Vector3();
        gene_body2[0].getWorldPosition(wpos2);
        var intersects2 = [{point:wpos2}];
        showResults2(intersects2, this.scene2);
      }
    }.bind(this);
    
    var starMeasureByGene = function(val){
      if(val==measureparam['gene 1']){
        if(measureparam['gene 2']!=''){
          checkGene(measureparam['gene 2']);
        }
        if(measureparam['gene 1']!=''){
          checkGene(measureparam['gene 1']);
        }
      }else{
        if(measureparam['gene 1']!=''){
          checkGene(measureparam['gene 1']);
        }
        if(measureparam['gene 2']!=''){
          checkGene(measureparam['gene 2']);
        }
      }
    }
    
    measureGUI.close();
  }
  
  setGUI(){
    // camera ratio GUI
    this.setCameraGUI();
    
    // search GUI
    this.setSearchGUI();
    
    // animate GUI
    this.setAnimatGUI();
    
    // exporter GUI
    this.setExporterGUI();
    
    // measurment GUI
    this.setMeasureGUI();

    // background color gui
    this.setBackgroundColorGUI();
    
    // light gui
    this.setLightGUI();
  }
  
  rotateXYZ(xyz, val){
    if(this.animateparam.linked){
      this.scene.rotation[xyz] = val;
      this.sceneBottom.rotation[xyz] = val;
      this.scene2.rotation[xyz] = val;
      this.sceneBottom2.rotation[xyz] = val;
    } else {
      if(this.animateparam.scene=='left'){
        this.scene.rotation[xyz] = val;
        this.sceneBottom.rotation[xyz] = val;
      }else{
        this.scene2.rotation[xyz] = val;
        this.sceneBottom2.rotation[xyz] = val;
      }
    }
  }
  
  flipXYZ(){
    const scale = new THREE.Vector3(1, 1, 1);
    if (this.animateparam.flip=='x') {
        scale.y = -1;
        scale.z = -1;
    }
    if (this.animateparam.flip=='y') {
        scale.x = -1;
        scale.z = -1;
    }
    if (this.animateparam.flip=='z') {
        scale.x = -1;
        scale.y = -1;
    }
    if(this.animateparam.linked){
      this.objects.scale.multiply(scale);
      this.objects2.scale.multiply(scale);
      this.objectsBottom.scale.multiply(scale);
      this.objectsBottom2.scale.multiply(scale);
    }else{
      if(this.animateparam.scene=='left'){
        this.objects.scale.multiply(scale);
        this.objectsBottom.scale.multiply(scale);
      }else{
        this.objects2.scale.multiply(scale);
        this.objectsBottom2.scale.multiply(scale);
      }
    }
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
    this.slider.style.top = this.container.offsetTop +
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
      var offset = this.container.offsetTop;
      this.sliderPos = Math.max( 0, Math.min( this.height, e.pageY -
        offset ) );
      this.slider.style.top = this.sliderPos + offset + 'px';
    }.bind(this);
    
    this.slider.style.touchAction = 'none'; // disable touch scroll
    this.slider.addEventListener( 'pointerdown', onPointerDown );
  }
  
  setBackground(x){
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
      
      this.titleBox.style.color = '#'+new THREE.Color(
        1-x.background.r[0],
        1-x.background.g[0],
        1-x.background.b[0]
      ).getHexString();
      
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
      
      this.titleBox2.style.color = '#'+new THREE.Color(
        1-x.background.r[2],
        1-x.background.g[2],
        1-x.background.b[2]
      ).getHexString();
    }
  }
  
  setOverlay(x){
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
  }
  
  setSecondTitlePosition(){
    this.titleBox2.style.top = this.container.offsetTop + 2 +'px';
    this.titleBox2.style.left = this.container.offsetLeft + this.width/2 + 2 + 'px';
  }
  
  setSideBySide(x){
    if('sideBySide' in x){
      this.sideBySide = x.sideBySide;
      if(x.sideBySide){
        this.camera.aspect = this.width/this.height;
        this.camera2.aspect = this.width/this.height;
        this.camera.updateProjectionMatrix();
        this.camera2.updateProjectionMatrix();
        this.labelRenderer.setSize( this.width/2, this.height );
        this.labelRenderer2.setSize( this.width/2, this.height );
        this.labelRenderer2.domElement.style.left = this.width/2+'px';
        this.container.insertBefore(this.labelRenderer2.domElement, this.slider);
        this.bckcolGUI.controllers[4].show();
        this.bckcolGUI.controllers[5].show();
        this.animateLinkedGUI.show();
        if('title' in x){
           this.titleBox2.innerText = x.title[1];
           this.setSecondTitlePosition();
        }
      }
    }
  }
  
  setDefaultValues(x){
    if('maxRadius' in x){
      this.maxRadius = x.maxRadius;
    }
    if('maxLineWidth' in x){
      this.maxLineWidth = x.maxLineWidth;
    }
  }
  
  setMainTitle(x){
    if('title' in x){
       this.titleBox.innerText = x.title[0];
       this.titleBox.style.top = this.container.offsetTop + 2 +'px';
       this.titleBox.style.left = this.container.offsetLeft + 2+ 'px';
    }
  }
  
  create_plot(x){
    //console.log(x);
    //const twoPi = Math.PI * 2;
    //x is a named array
    this.setBackground(x);
    this.setDefaultValues(x);
    this.setMainTitle(x);
    this.setSideBySide(x);
    this.setOverlay(x);
    
    const arrowLayer = [];
    const groupFolder = this.gui.addFolder('Group setting');
    const groupFolderObj = {};
    const groupParamObj = {};
    var toggleAllArrowGUI = null;
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
      };
      toggleAllArrowGUI = layerFolder.add(labelLayer, 'Toggle all arrows').hide();
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
    function initNewMesh(obj, ele, offset={x:0,y:0,z:0}){
      const matrix = new THREE.Matrix4();
      const color = new THREE.Color();
      ele.recenter = true;
      for ( let i = 0; i < obj.count; i ++ ) {
        matrix.setPosition( ele.positions[i*3] - offset.x,
                            ele.positions[i*3+1] - offset.y,
                            ele.positions[i*3+2] - offset.z);
        obj.setMatrixAt( i, matrix );
        if(ele.colors.length==ele.positions.length){
          obj.setColorAt( i, color.setRGB(
                ele.colors[i*3],
                ele.colors[i*3+1],
                ele.colors[i*3+2]
              ) );
        }else{//same color for all elements
          obj.setColorAt( i, color.setRGB(
                ele.colors[0],
                ele.colors[1],
                ele.colors[2]
              ) );
        }
      }
      if(typeof(ele.rotation) != 'undefined'){
        obj.rotation.x = ele.rotation.x;
        obj.rotation.y = ele.rotation.y;
        obj.rotation.z = ele.rotation.z;
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
         k!='overlay' && k!='sideBySide' &&
         k!='title'){
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
        // get the center of the object
        let center = new THREE.Vector3(0, 0, 0);
        for ( let i =0; i<len; i++){
          center.x += ele.positions[i*3];
          center.y += ele.positions[i*3+1];
          center.z += ele.positions[i*3+2];
        }
        center.multiplyScalar(1/len);
        ele.recenter = false;
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
              opacity: param.opacity,
              transparent:true
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
            initNewMesh(obj, ele, center);
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
              if(this.symbols.indexOf(ele.label[i])==-1){
                this.symbols.push(ele.label[i]);
              }
              labelDiv.textContent = ele.label[i];
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
              css2obj.name = ele.label[i];
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
            initNewMesh(obj, ele, center);
            obj.layers.set(this.getLayer(ele.tag));
            folder.add(param, 'radius', 0, this.maxRadius).onChange( function( val) {
              octahedrondata.radius = val;
              updateGroupGeometry(obj, new THREE.OctahedronGeometry(
                octahedrondata.radius));
            });
            break;
          case 'polygon':
            ele.recenter <- false;
            geometry.setIndex(ele.indices);
            geometry.setAttribute('position', new THREE.Float32BufferAttribute(ele.positions, 3));
            geometry.computeVertexNormals();
            const materials = [];
            var oldColor = new THREE.Color(ele.colors[0], ele.colors[1], ele.colors[2]);
            var oldOpacity = ele.alpha[0];
            var oldI = 0;
            for(let i=1; i<ele.alpha.length; i++){
              if(ele.colors[i*3]==ele.colors[oldI*3] &&
                  ele.colors[i*3+1]==ele.colors[oldI*3+1] &&
                  ele.colors[i*3+2]==ele.colors[oldI*3+2] &&
                  ele.alpha[i]==oldOpacity){
                    continue;
              }else{
                var j = materials.push(new THREE.MeshStandardMaterial( { color: oldColor, opacity:oldOpacity, transparent:true, side:THREE.DoubleSide } ));
                geometry.addGroup(oldI*3, (oldI+i)*3, j-1);
                oldI = i;
                oldColor = new THREE.Color(ele.colors[i*3], ele.colors[i*3+1], ele.colors[i*3+2]);
                oldOpacity = ele.alpha[i];
              }
            }
            var j = materials.push(new THREE.MeshStandardMaterial( { color: oldColor, opacity:oldOpacity, transparent:true, side:THREE.DoubleSide } ));
            geometry.addGroup(oldI*3, ele.alpha.length*3, j-1);
            obj = new THREE.Mesh(geometry, materials);
            obj.layers.set(this.getLayer(ele.tag));
            break;
          case 'tetrahedron':
            param.radius = ele.radius;
            const tetrahedrondata = {
              radius: ele.radius
            };
            geometry = new THREE.TetrahedronGeometry(
              tetrahedrondata.radius);
            obj = new THREE.InstancedMesh( geometry, material, len );
            initNewMesh(obj, ele, center);
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
                x: center.x + 0.5 * (geometry.boundingBox.max.x - geometry.boundingBox.min.x ),
                y: center.y + 0.5 * (geometry.boundingBox.max.y - geometry.boundingBox.min.y ),
                z: center.z + 0.5 * (geometry.boundingBox.max.z - geometry.boundingBox.min.z )
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
            initNewMesh(obj, ele, center);
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
          material.color = val;
        });
        folder.add(param, 'opacity', 0, 1).onChange( function( val ){
          material.opacity = val;
        });
        folder.add(param, 'transparent').onChange( function( val ){
          material.transparent = val;
        });
        folder.close();
        // add obj to a parent container
        let objContainer = new THREE.Group();
        objContainer.add(obj);
        if(ele.recenter) objContainer.position.add(center);
        this.materials.push(material);
            const rotationdata = {
              'rotate x' : 0,
              'rotate y' : 0,
              'rotate z' : 0
            } 
            folder.add(rotationdata, 'rotate x', 0, 2*Math.PI).onChange( function( val) {
              obj.rotation.x = val;
            });
            folder.add(rotationdata, 'rotate y', 0, 2*Math.PI).onChange( function( val) {
              obj.rotation.y = val;
            });
            folder.add(rotationdata, 'rotate z', 0, 2*Math.PI).onChange( function( val) {
              obj.rotation.z = val;
            });
        if(ele.layer=='top'){
          if(ele.side=='left'){
              this.objects.add(objContainer);
          }else{
              this.objects2.add(objContainer);
          }
        }else{
          if(ele.side=='left'){
              this.objectsBottom.add(objContainer);
          }else{
              this.objectsBottom2.add(objContainer);
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
    if(arrowLayer.length>0){
      toggleAllArrowGUI.show();
    }
    this.gui.close();
    this.setAutocompleteDatalist();
  }
  
  makeOrthographicCamera() {
    const halfFovV = THREE.MathUtils.DEG2RAD * 45 * 0.5;
    const halfFovH = Math.atan( ( this.width / this.height ) * Math.tan( halfFovV ) );
    const halfW = this.perspectiveDistance * Math.tan( halfFovH );
    const halfH = this.perspectiveDistance * Math.tan( halfFovV );
    const newCamera = new THREE.OrthographicCamera( - halfW, halfW, halfH, - halfH, this.near, this.far );
    newCamera.layers.enableAll();
    return newCamera;
  }
  
  makePerspectiveCamera() {
    const aspect = this.width / this.height;
    const newCamera = new THREE.PerspectiveCamera( this.fov, aspect, this.near, this.far );
    newCamera.layers.enableAll();
    return newCamera;
  }
  
  makeCamera(camera) {
    if ( camera.type == 'OrthographicCamera' ) {
      const halfFovV = THREE.MathUtils.DEG2RAD * 45 * 0.5;
      const halfFovH = Math.atan( ( this.width / this.height ) * Math.tan( halfFovV ) );
      const halfW = this.perspectiveDistance * Math.tan( halfFovH );
      const halfH = this.perspectiveDistance * Math.tan( halfFovV );
      camera.left = - halfW;   
      camera.right = halfW;
      camera.top = halfH;
      camera.bottom = - halfH;
    } else if ( camera.type == 'PerspectiveCamera' ) {
      camera.aspect = this.width / this.height;
    }
  }
  
  setCamera() {
    var camera;
    if ( this.cameraparam.type == 'Orthographic' ) {
      camera = this.makeOrthographicCamera();
      camera.position.set( 0, 0, this.orthographicDistance );
    } else if ( this.cameraparam.type == 'Perspective' ) {
      camera = this.makePerspectiveCamera();
      camera.position.set( 0, 0, this.perspectiveDistance );
    }
    camera.layers.enableAll();
    return(camera);
  }
  
  makeControls(camera, labelRenderer, scene){
    const newControls = new ArcballControls( camera, labelRenderer.domElement, scene );
    newControls.enableDamping = true;
    newControls.minDistance = this.near*2;
    newControls.maxDistance = this.far/2;
    newControls.setGizmosVisible(false);
    return(newControls);
  }
  
  linkPan(){
    this.container.parentElement.addEventListener('wheel', (event)=>{
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
  }
  
  addGizmos(){
    this.controls.addEventListener('start', ()=>{
      this.controls.setGizmosVisible(true);
    });
    this.controls.addEventListener('end', ()=>{
      this.controls.setGizmosVisible(false);
    });
    this.controls2.addEventListener('start', ()=>{
      this.controls2.setGizmosVisible(true);
    });
    this.controls2.addEventListener('end', ()=>{
      this.controls2.setGizmosVisible(false);
    });
  }
  
  linkCamera(cam1, cam2, ctl1, ctl2){
    if(this.animateparam.linked){
        cam2.position.copy( cam1.position );
        cam2.rotation.copy( cam1.rotation );
        cam2.zoom = cam1.zoom;
        ctl2.target.copy( ctl1.target );
    }
  }
  
  linkCam1ToCam2(){
    this.linkCamera(this.camera, this.camera2, this.controls, this.controls2);
  }
  linkCam2ToCam1(){
    this.linkCamera(this.camera2, this.camera, this.controls2, this.controls);
  }
  
  removeLinkedControls(){
    this.controls.removeEventListener('change', this.linkCam1ToCam2);
    this.controls2.removeEventListener('change', this.linkCam2ToCam1);
  }
  
  linkControls(){
    this.controls.addEventListener('change', this.linkCam1ToCam2.bind(this));
    this.controls2.addEventListener('change', this.linkCam2ToCam1.bind(this));
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
      this.makeCamera(this.camera);
      this.makeCamera(this.camera2);
      //this.camera.aspect = width/height;
      //this.camera2.aspect = width/height;
      this.camera2.updateProjectionMatrix();
      this.labelRenderer.setSize( width/2, height );
      this.labelRenderer2.setSize( width/2, height );
      this.labelRenderer2.domElement.style.top = '-'+2*height+'px';
      this.labelRenderer2.domElement.style.left = width/2+'px';
      this.setSecondTitlePosition();
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
    this.setExporterGUIpara();
  }
  
  animate(){
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
    
    this.camera.updateMatrixWorld();
    
    //this.gpuPanel.startQuery();
    if(this.overlay){
      if(this.sideBySide){
        this.controls2.update();
        this.camera2.updateMatrixWorld();
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
        this.camera2.updateMatrixWorld();
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
        if(this.insetCamera){
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
