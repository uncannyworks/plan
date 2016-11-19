---
title: Forward Into.. Javascript?
author: Chris Johnston
---

A NodeJS SDK for Armored Bits.

### What is Armored Bits?

Armored Bits is a competitive, multiplayer, online game where players hone their programming skills by writing AI for massive autonomous robots. Players have no direct control of their machines, instead they are tasked with reading input from scanners, processing communication streams from teammates, and analyzing their machine's state to make intelligent decisions and win victory in combat. This is all accomplished by sending network messages to the Game Server hosting the match and processing the responses received. The goal of the final product is to allow players to choose any programming language they want to code their AI and as long as said AI can correctly communicate with the Game Server over a network connection. At these early stages of development we've chosen to build an SDK targeting Javascript with NodeJS. In this post we'll cover the reasons behind our decisions as well as dig into some of the internals of this SDK. 

### Why an SDK at all?

Raw communication and event handling with the game server can be overwhelming for the novice level programmer. The goal of the SDK is to provide a simple interface into these complex functions.

### Why NodeJS?

Node provides a stable and consistent runtime that is easily installed on many operating systems. With a quick visit to their [website](https://nodejs.org/en/download/package-manager/) you can see that, regardless of the operating system, installation can usually be done in a single terminal command. In addition to simple installation, running a NodeJS program is as simple as typing:

```
$ node mycode.js
```

### Why Javascript?

 Javascript is both powerful and accessible. With the many learning [resources available](https://www.google.com/#q=learn+javascript), a fortune's worth of [Stack Overflow](http://stackoverflow.com/questions/tagged/javascript?sort=votes) answers, thriving [communities](http://irc.lc/freenode/javascript), and the shallow learning curve it makes sense that Javascript would be the [first choice](http://redmonk.com/sogrady/2016/02/19/language-rankings-1-16/) for many people wanting to learn programming.

### Talking to the Game Server

We'll begin by exploring how the SDK simplifies sending a request to the game server. We'll examine the case where a player wants to fire a weapon. Lets have a look at the internals of `sdk.fire_weapon(warmachine.weapons[0])` to see what the SDK does for us. Before we move forward note that `warmachine` is an object that we obtain by asking the Game Server for our war machine's current state in the game world. We'll go into more detail on how this is accomplished in the next post, for now just understand that we got `warmachine` from a previous step not covered here. 

<pre><code class="javascript">
  this.fire_weapon = function(weaponStruct) {
    this.send_weapon_request(weaponStruct.location.locationType,
      weaponStruct.location.parentId,
      weaponStruct.location.positionId,
      this.COMPONENT_STATE.Active,
      this.WEAPON_FIRE_STATE.Fire
    );
  }
</code></pre>

First we must consider the parameter `weaponStruct`. `warmachine` has an array of weapon objects and we're passing in the first element of that array. The complete structure of these objects can be found in the protobuf definitions [here](https://github.com/uncannyworks/armoredbits.protobufs/blob/master/slug.proto#L93) and [here](https://github.com/uncannyworks/armoredbits.protobufs/blob/master/query.proto). The function then deconstructs the `weaponStruct` parameter and passes off the heavy lifting to `send_weapon_request` which we will dig into next.

<pre><code class="javascript">
  this.send_weapon_request = function(locationType, parentId, positionId, state, fireState) {
    var p = protobufBuilder.build("SlugCommitWeaponRequest");
    var l = protobufBuilder.build("SlugQueryLocationMessage");
    var loc = new l(locationType, parentId, positionId);
    var m = new p(loc, state, fireState);
    var mg = _build_message(this.MESSAGE_CODES.SlugCommitWeaponRequest, m);
    _send_message(mg);
  }
</code></pre>

In general the function takes the parameters and builds the message that will be shot over the network connection. `protobufBuilder.build` comes from the [protobufjs](https://www.npmjs.com/package/protobufjs) library and facilitates turning javascript objects into raw bytes and back again using structures defined by [Google Protocol Buffers](https://developers.google.com/protocol-buffers). The first two lines of the function load up the blueprints that will be used to make the objects representing the protobuf messages. The third and fourth lines actually perform the creation using the parameters provided. The reason we need two blueprints is because `SlugCommitWeaponRequest`, [as you can see in the proto file](https://github.com/uncannyworks/armoredbits.protobufs/blob/master/commit.proto#L48), contains a nested message. `_build_message` is responsible for taking the javascript object we've created and processing it into an array of bytes ready to be thrown on the wire. Next lets take a look at some of the parts of `_build_message` and what it is doing for us.

<pre><code class="javascript">
var _build_message = function(code, message) {
  //... code above omitted for brevity ...
  buff = message.encode();
  var len = buff.toArrayBuffer().byteLength;

  var messageLengthBytes = new ArrayBuffer(2); // an Int16 takes 2 bytes
  var view = new DataView(messageLengthBytes);
  view.setUint16(0, len, false); // byteOffset = 0; litteEndian = false
  var bb = protobuf.ByteBuffer.wrap(messageLengthBytes);
  buff.prepend(bb);

  var messageCode = new Uint8Array(1);
  messageCode[0] = code;
  bb = protobuf.ByteBuffer.wrap(messageCode);
  buff.prepend(bb);
  //... code below omitted for brevity ...
}
</code></pre>

The game server expects messages to consist of a 1 byte message id, followed by a 2 byte payload length, then the payload. Even after omitting some code the above may look like a lot, so lets break it down.

<pre><code class="javascript">
  buff = message.encode();
</code></pre>

This is the whole reason we asked [protobufjs](https://www.npmjs.com/package/protobufjs) to join the ride. It returns a buffer full of the bytes that will make up our payload.

<pre><code class="javascript">
  var messageLengthBytes = new ArrayBuffer(2); // an Int16 takes 2 bytes
  var view = new DataView(messageLengthBytes);
  view.setUint16(0, len, false); // byteOffset = 0; litteEndian = false
  var bb = protobuf.ByteBuffer.wrap(messageLengthBytes);
  buff.prepend(bb);
  
  var messageCode = new Uint8Array(1);
  messageCode[0] = code;
  bb = protobuf.ByteBuffer.wrap(messageCode);
  buff.prepend(bb);
</code></pre>

The next step is to take the payload length and message type id, which are integers, and translate them into a 2 and 1 byte array respectively. [DataView](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) is how we are able to do that. We then prepend them to the payload to get our final message that will go over the wire. The final step is to fire the message over the wire, and this is done by `_send_message`.

Internally `_send_message` manages the requests so that they go out as quickly as possible but don't flood the buffer, we'll go into greater detail at another time. We've basically covered the process for sending a request to the game server. While each message is a bit different in its payload they all go through the same digestion process.

### I can do it myself!

Obviously there is nothing preventing a player from implementing their own solution to communicate with the game server, and we encourage you to do so! In addition to providing new programmers with a tool to quickly start programming the AI for their War Machine this SDK also acts as an example for the more advanced programmers that may be interested in rolling their own solutions.

In the next post we'll go over how the SDK handles responses from the game server. It will cover how the SDK manages and simplifies retrieving your war machine's state, handling Game State changes, and more. 
