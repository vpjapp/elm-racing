import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";
import { Engine, World, Bodies, Body, Vector } from "matter-js";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});
app.ports.elmToJs.subscribe(({ dataType, payload }) => {
  //console.log("elmToJs MESSAGE", dataType, payload);

  const res = handlers[dataType] && handlers[dataType](payload);
  //console.log('res', res);
  if (res) {
    app.ports.jsToElm.send({ dataType: "UpdatedPhysics", payload: res });
  } else {
    alert(`Unknown dataType ${dataType}`);
  }
});

const engine = Engine.create();
const world = engine.world;
world.gravity.y = 0;

const bodies = [];

registerServiceWorker();

/*******************************/
/* Functions
/*******************************/
const addBodies = (bodySpecs) => {
  World.clear(world);
  bodies.length = 0;
  bodySpecs.forEach((bodySpec) => {
    console.log("Adding body: ", bodySpec);
    const {
      x,
      y,
      width,
      height,
      mass,
      id,
      rotation,
      velocity,
      type_,
    } = bodySpec;
    //console.log('Adding body', {x, y, width, height, mass, id, rotation, velocity})
    const body = createBody(x, y, width, height, type_);
    body.width = width;
    body.height = height;
    Body.setMass(body, mass);
    body.label = id;
    body.type_ = type_;
    //console.log ('Rotten Angle', rotation)
    Body.setAngle(body, rotation);
    //console.log ('Rotten Angle2', body.angle)
    //Body.setAngularVelocity(body, 0);
    Body.setVelocity(body, velocity);
    //body.angle = rotation;
    //console.log('Velocity', velocity)
    //body.velocity = velocity;
    bodies.push(body);
    World.add(world, body);
  });
  //console.log('world', world);
  const res = toBodySpecs(bodies);
  return res;
};

const createBody = (x, y, width, height, type) => {
  switch (type) {
    case "circle":
      return Bodies.circle(x, y, width, height);
    case "car":
    default:
      return Bodies.rectangle(x, y, width, height);
  }
};

const toBodySpecs = (bodies) => {
  return bodies.map((body) => {
    //console.log('Body toBodySpec', body)
    const bodySpec = {
      x: Math.round(body.position.x),
      y: Math.round(body.position.y),
      width: body.width,
      height: body.height,
      rotation: body.angle,
      mass: body.mass,
      id: body.label,
      velocity: body.velocity,
      type_: body.type_,
    };
    //console.log('bodySpec', bodySpec)
    return bodySpec;
  });
};

const returnValue = (dataType, payload) => ({ dataType, payload });

const updatePhysics = ({ delta, forces = [] }) => {
  // forces: {id: string, force: {x:float, y:float }}
  forces.forEach((force) => {
    const car = bodies.filter((b) => b.label === force.id)[0];

    if (car) {
      if (force.force) {
        Body.applyForce(
          car,
          { x: car.position.x, y: car.position.y },
          force.force
        );
      }

      if (Vector.magnitude(force.force) > 0) {
        const angle = Vector.angle(force.force, { x: 0, y: 0 }) + Math.PI / 2;
        Body.setAngle(car, angle);
      }
    } else {
      alert(`Could not find car ${force.id}`);
    }
  });

  if (world.bodies.length < 1) {
    return [];
  }
  Engine.update(engine, delta);

  const res = toBodySpecs(bodies);
  return res;
};

const handlers = {
  PhysicsUpdate: updatePhysics,
  AddBodies: addBodies,
};
