function (el, x, data) {
  var badge = document.createElement("div");
  badge.textContent = "turbines v11…";
  badge.style.cssText = "position:absolute;bottom:8px;right:8px;z-index:9;background:#111;color:#fff;padding:4px 8px;font:12px sans-serif;";
  el.appendChild(badge);
  console.log("WINDFARMGA_TURBINES_V11", data.three_url, data.obj_url, data.turbines && data.turbines.length);

  function hasThree() {
    return window.THREE &&
      typeof THREE.Mesh === "function" &&
      typeof THREE.WebGLRenderer === "function";
  }

  function loadScript(src) {
    return new Promise(function (resolve, reject) {
      if (hasThree()) {
        resolve();
        return;
      }
      if (window.THREE && !hasThree()) {
        try { delete window.THREE; } catch (e) { window.THREE = undefined; }
      }
      var s = document.createElement("script");
      s.src = src;
      s.onload = function () {
        if (!hasThree()) {
          reject(new Error("THREE incomplete after " + src));
          return;
        }
        resolve();
      };
      s.onerror = function () { reject(new Error(src)); };
      document.head.appendChild(s);
    });
  }

  function loadThree() {
    var urls = [];
    if (data.three_url) {
      urls.push(data.three_url);
    }
    urls.push("https://cdn.jsdelivr.net/npm/three@0.149.0/build/three.min.js");
    var i = 0;
    function next() {
      console.log("WINDFARMGA_TURBINES_V10 load three", urls[i]);
      return loadScript(urls[i]).catch(function (err) {
        console.warn("WINDFARMGA_TURBINES_V10 three miss", err);
        i += 1;
        if (i >= urls.length) {
          throw new Error("three.min.js r149");
        }
        return next();
      });
    }
    return next();
  }

  function mapReadyForObj(map) {
    var hasTerrain = false;
    try {
      hasTerrain = !!(map.getTerrain && map.getTerrain());
    } catch (e) {
      hasTerrain = false;
    }
    var hasLabels = map.getLayer && map.getLayer("turbine-labels");
    return hasTerrain && hasLabels;
  }

  function whenStyleReady(map, fn) {
    var done = false;
    var run = function (why) {
      if (done || !mapReadyForObj(map)) {
        return;
      }
      done = true;
      console.log("WINDFARMGA_TURBINES_V10 terrain+layers ready via", why);
      fn();
    };
    map.on("style.load", function () { run("style.load"); });
    map.once("idle", function () { run("idle"); });
    var n = 0;
    var t = setInterval(function () {
      n += 1;
      run("poll-" + n);
      if (done) {
        clearInterval(t);
      } else if (n > 120) {
        clearInterval(t);
        console.warn("WINDFARMGA_TURBINES_V10 ready timeout, adding anyway");
        done = true;
        fn();
      }
    }, 100);
  }

  function elevOf(map, t, exaggeration) {
    if (typeof t.elev === "number" && isFinite(t.elev)) {
      return t.elev * (exaggeration || 1);
    }
    try {
      var q = map.queryTerrainElevation({ lng: t.lng, lat: t.lat });
      if (typeof q === "number" && isFinite(q)) {
        return q;
      }
    } catch (e) {}
    return 0;
  }

  function zCut(verts) {
    var zs = [];
    for (var i = 2; i < verts.length; i += 3) {
      zs.push(verts[i]);
    }
    zs.sort(function (a, b) { return a - b; });
    var best = 0;
    var cut = zs[zs.length - 1] + 1;
    for (var j = 1; j < zs.length; j++) {
      var gap = zs[j] - zs[j - 1];
      if (gap > best) {
        best = gap;
        cut = (zs[j - 1] + zs[j]) / 2;
      }
    }
    return best > 500 ? cut : zs[zs.length - 1] + 1;
  }

  function parseObj(text) {
    var verts = [];
    var faces = [];
    var lines = text.split("\n");
    for (var i = 0; i < lines.length; i++) {
      var ln = lines[i].trim();
      if (ln.charAt(0) === "v" && ln.charAt(1) === " ") {
        var p = ln.split(/\s+/);
        verts.push(+p[1], +p[2], +p[3]);
      } else if (ln.charAt(0) === "f" && ln.charAt(1) === " ") {
        var idx = ln.split(/\s+/).slice(1).map(function (tok) {
          return parseInt(tok.split("/")[0], 10) - 1;
        });
        for (var k = 1; k < idx.length - 1; k++) {
          faces.push(idx[0], idx[k], idx[k + 1]);
        }
      }
    }
    if (!faces.length) {
      throw new Error("OBJ has no faces");
    }
    var cut = zCut(verts);
    var kept = [];
    for (var f = 0; f < faces.length; f += 3) {
      var a = faces[f], b = faces[f + 1], c = faces[f + 2];
      if (verts[a * 3 + 2] > cut || verts[b * 3 + 2] > cut || verts[c * 3 + 2] > cut) {
        continue;
      }
      kept.push(a, b, c);
    }
    if (!kept.length) {
      kept = faces;
    }
    var pos = new Float32Array(kept.length * 3);
    for (var t = 0; t < kept.length; t++) {
      var vi = kept[t] * 3;
      pos[t * 3] = verts[vi];
      pos[t * 3 + 1] = verts[vi + 1];
      pos[t * 3 + 2] = verts[vi + 2];
    }
    var geom = new THREE.BufferGeometry();
    geom.setAttribute("position", new THREE.BufferAttribute(pos, 3));
    geom.computeVertexNormals();
    var mesh = new THREE.Mesh(
      geom,
      new THREE.MeshBasicMaterial({ color: 0x111111, side: THREE.DoubleSide })
    );
    var g = new THREE.Group();
    g.add(mesh);
    return g;
  }

  function stickTurbine(hub, rotor) {
    var g = new THREE.Group();
    var mat = new THREE.MeshBasicMaterial({ color: 0x111111, side: THREE.DoubleSide });
    var tower = new THREE.Mesh(new THREE.CylinderGeometry(1.4, 2.4, hub, 12), mat);
    tower.position.y = hub / 2;
    g.add(tower);
    var nacelle = new THREE.Mesh(new THREE.BoxGeometry(8, 3, 3), mat);
    nacelle.position.set(2, hub, 0);
    g.add(nacelle);
    var disk = new THREE.Mesh(
      new THREE.CircleGeometry(rotor || hub * 0.45, 24),
      new THREE.MeshBasicMaterial({ color: 0x111111, side: THREE.DoubleSide })
    );
    disk.position.set(6, hub, 0);
    disk.rotation.y = Math.PI / 2;
    g.add(disk);
    return g;
  }

  function fitTemplate(raw, hub) {
    raw.rotation.x = Math.PI / 2;
    raw.updateMatrixWorld(true);
    var box = new THREE.Box3().setFromObject(raw);
    var size = new THREE.Vector3();
    box.getSize(size);
    var h = size.y > 1e-6 ? size.y : 1;
    var sc = hub / h;
    raw.scale.setScalar(sc);
    raw.updateMatrixWorld(true);
    box.setFromObject(raw);
    var mid = new THREE.Vector3();
    box.getCenter(mid);
    raw.position.set(-mid.x, -box.min.y, -mid.z);
    var g = new THREE.Group();
    g.add(raw);
    addHubRotor(g, hub, hub * 0.45);
    console.log("WINDFARMGA_TURBINES_V11 fit", { h: h, sc: sc, hub: hub });
    return g;
  }

  function addHubRotor(g, hub, rotor) {
    var mat = new THREE.MeshBasicMaterial({ color: 0x111111, side: THREE.DoubleSide });
    var nacelle = new THREE.Mesh(new THREE.BoxGeometry(10, 3.2, 3.2), mat);
    nacelle.position.set(3, hub, 0);
    g.add(nacelle);
    var disk = new THREE.Mesh(
      new THREE.CircleGeometry(rotor, 28),
      new THREE.MeshBasicMaterial({
        color: 0x111111,
        side: THREE.DoubleSide,
        transparent: true,
        opacity: 0.55
      })
    );
    disk.position.set(8, hub, 0);
    disk.rotation.y = Math.PI / 2;
    g.add(disk);
    var bladeMat = new THREE.MeshBasicMaterial({ color: 0x111111, side: THREE.DoubleSide });
    for (var i = 0; i < 3; i++) {
      var blade = new THREE.Mesh(new THREE.BoxGeometry(1.1, rotor, 0.35), bladeMat);
      blade.position.set(8, hub, 0);
      blade.rotation.x = i * Math.PI * 2 / 3;
      blade.translateY(rotor * 0.5);
      g.add(blade);
    }
  }

  function metersEastNorth(fromLngLat, toLngLat) {
    var a = maplibregl.MercatorCoordinate.fromLngLat(fromLngLat);
    var b = maplibregl.MercatorCoordinate.fromLngLat(toLngLat);
    var m = a.meterInMercatorCoordinateUnits();
    return { east: (b.x - a.x) / m, north: (a.y - b.y) / m };
  }

  function placeClones(layer, data) {
    var origin = data.turbines[0];
    var elev0 = elevOf(layer.map, origin, data.exaggeration);
    layer.originElev = elev0;
    if (layer.group) {
      layer.scene.remove(layer.group);
    }
    layer.group = new THREE.Group();
    data.turbines.forEach(function (t) {
      var elev = elevOf(layer.map, t, data.exaggeration);
      var d = metersEastNorth(origin, t);
      var one = layer.template.clone(true);
      one.position.set(d.east, elev - elev0, -d.north);
      one.rotation.y = -Math.PI / 2 - data.yaw_rad;
      layer.group.add(one);
    });
    layer.scene.add(layer.group);
    console.log("WINDFARMGA_TURBINES_V12 placed", data.turbines.length, "elev0", elev0);
    layer.map.triggerRepaint();
  }

  function addLayer(map, data) {
    if (map.getLayer && map.getLayer("turbine-obj-v10")) {
      return;
    }
    var origin = data.turbines[0];
    var layer = {
      id: "turbine-obj-v10",
      type: "custom",
      renderingMode: "3d",
      origin: origin,
      originElev: 0,
      onAdd: function (map, gl) {
        this.map = map;
        badge.textContent = "turbines v10 onAdd";
        var self = this;
        loadThree()
          .then(function () {
            self.camera = new THREE.Camera();
            self.scene = new THREE.Scene();
            self.renderer = new THREE.WebGLRenderer({
              canvas: map.getCanvas(),
              context: gl,
              antialias: true
            });
            self.renderer.autoClear = false;
            var rotor = data.hub * 0.45;
            var ready = function (raw, fromObj) {
              self.template = fromObj ? fitTemplate(raw, data.hub) : raw;
              placeClones(self, data);
              badge.textContent = "turbines v10 (" + data.turbines.length + ")";
            };
            return fetch(data.obj_url).then(function (res) {
              if (!res.ok) {
                throw new Error("OBJ HTTP " + res.status);
              }
              return res.text();
            }).then(function (text) {
              ready(parseObj(text), true);
            }).catch(function (err) {
              console.warn("windfarmGA OBJ fallback to stick turbines", err);
              badge.textContent = "turbines v10 stick";
              ready(stickTurbine(data.hub, rotor), false);
            });
          })
          .catch(function (err) {
            console.error("windfarmGA three.js failed", err);
            badge.textContent = "turbines v10 FAILED";
          });
      },
      render: function (gl, args) {
        if (!this.renderer || !this.map) {
          return;
        }
        var main = null;
        if (args && args.defaultProjectionData && args.defaultProjectionData.mainMatrix) {
          main = args.defaultProjectionData.mainMatrix;
        } else if (args && args.modelViewProjectionMatrix) {
          main = args.modelViewProjectionMatrix;
        } else if (args && args.length) {
          main = args;
        }
        if (!main || !main.length) {
          return;
        }
        var elev = this.originElev || 0;
        var merc = maplibregl.MercatorCoordinate.fromLngLat(
          [this.origin.lng, this.origin.lat],
          elev
        );
        var s = merc.meterInMercatorCoordinateUnits();
        var rotationX = new THREE.Matrix4().makeRotationAxis(
          new THREE.Vector3(1, 0, 0),
          Math.PI / 2
        );
        var m = new THREE.Matrix4().fromArray(main);
        var l = new THREE.Matrix4()
          .makeTranslation(merc.x, merc.y, merc.z)
          .scale(new THREE.Vector3(s, -s, s))
          .multiply(rotationX);
        this.camera.projectionMatrix = m.multiply(l);
        this.renderer.resetState();
        this.renderer.render(this.scene, this.camera);
        this.map.triggerRepaint();
      }
    };
    console.log("WINDFARMGA_TURBINES_V10 addLayer");
    map.addLayer(layer);
  }

  function start() {
    var map = el.map;
    if (!map) {
      setTimeout(start, 80);
      return;
    }
    whenStyleReady(map, function () {
      addLayer(map, data);
    });
    map.on("style.load", function () {
      setTimeout(function () {
        if (mapReadyForObj(map) && !(map.getLayer && map.getLayer("turbine-obj-v10"))) {
          console.log("WINDFARMGA_TURBINES_V10 re-add after style.load");
          addLayer(map, data);
        }
      }, 200);
    });
  }
  start();
}
