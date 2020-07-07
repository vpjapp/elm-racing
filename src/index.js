import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";
import { Engine, World, Bodies, Body, Vector } from "matter-js";

const app = Elm.Main.init({
  node: document.getElementById("root")
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
const addBodies = bodySpecs => {
  World.clear(world);
  bodies.length = 0;
  bodySpecs.forEach(bodySpec => {
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
      type_
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

const toBodySpecs = bodies => {
  return bodies.map(body => {
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
      type_: body.type_
    };
    //console.log('bodySpec', bodySpec)
    return bodySpec;
  });
};

const returnValue = (dataType, payload) => ({ dataType, payload });

const updatePhysics = ({ delta, forces = [] }) => {
  /*
  Object.keys(forces).forEach(id => {
    forces[id].forEach(objectForce => {
      Body.applyForce(bodies[id], {x: body.x, y: body.y}, objectForce.vector);
    })
  })
  */
  const body = bodies[0];
  if (forces.length > 0 && bodies.length > 0) {
    let force = forces[0];

    const speed = Vector.magnitude(body.velocity);
    const accelerationMultiplier = getAcceleration(speed);

    force = Vector.normalise(force);
    force = Vector.mult(force, accelerationMultiplier);

    let force2 = forces[1];
    // force2 = Vector.normalise(force2);
    // force2 = Vector.mult(force2, 0.08);

    //console.log('BodiesForces', bodies, body.position.x, body.position.y);
    // console.log('Forces 0',force, body.position);
    // console.log('Forces 1',force2, body.position);
    Body.applyForce(body, { x: body.position.x, y: body.position.y }, force);
    Body.applyForce(body, { x: body.position.x, y: body.position.y }, force2);

    if (Vector.magnitude(force) > 0) {
      const angle = Vector.angle(force, { x: 0, y: 0 }) + Math.PI / 2;
      Body.setAngle(body, angle);
    }
  }

  if (world.bodies.length < 1) {
    return [];
  }
  //const velocity = world.bodies[0].velocity;
  //Body.setVelocity(world.bodies[0], {x: velocity.x *0.9, y: velocity.y*0.9})

  //console.log('VELOCITY:', world.bodies[0].velocity)
  //console.log('ANGLE:', world.bodies[0].angle)
  //console.log('POS:', world.bodies[0].position)
  Engine.update(engine, delta);
  //console.log('VELOCITY:', world.bodies[0].velocity)
  //console.log('ANGLE:', world.bodies[0].angle)
  //console.log('POS:', world.bodies[0].position)

  if (body) {
    //console.log('Rotten force 3', body.angle)
  }

  //console.log('update physics', forces);
  try {
    //console.err('world BODY', world.bodies[0]);
  } catch (e) {}
  const res = toBodySpecs(bodies);
  //console.log('PosseRes:', res);
  return res;
};

const getAcceleration = (speed) => {
  const maxSpeed = 10;
  const maxAcc = 1;
  const minAcc = 0.001;

  // let acc = 0.03;
  // if (speed > 5) {
  const  acc = 0.25;
  // }
  // if (speed < 2) {
  //   acc = 0.2;
  // } else if (speed < 5) {
  //   acc = 0.3
  // } else if (speed < maxSpeed) {
  //   acc = 0.1
  // } else {
  //   acc = 0.01
  // }

  // // rel speed = 0 ... 1
  // const relSpeed = Math.min(speed / maxSpeed, 1) + 0.0001;

  // const acc = (1 - relSpeed) * maxAcc;

  //console.log('SPEED', speed, 'acc', acc)

  return Math.max(acc, minAcc);
}

const handlers = {
  PhysicsUpdate: updatePhysics,
  AddBodies: addBodies
};
