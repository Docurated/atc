var DEBUG = {};

var KEY_HEIGHT = 20;
var KEY_SPACING = 10;
var RANK_GROUP_SPACING = 50;
var NODE_PADDING = 5;

function Graph() {
  this._nodes = {};
  this._edges = [];
};

Graph.prototype.setNode = function(id, value) {
  this._nodes[id] = value;
};

Graph.prototype.removeNode = function(id) {
  var node = this._nodes[id];

  for (var i in node._inEdges) {
    var edge = node._inEdges[i];
    var sourceNode = edge.source.node;
    var idx = sourceNode._outEdges.indexOf(edge);
    sourceNode._outEdges.splice(idx, 1);

    var graphIdx = this._edges.indexOf(edge);
    this._edges.splice(graphIdx, 1);
  }

  for (var i in node._outEdges) {
    var edge = node._outEdges[i];
    var targetNode = edge.target.node;
    var idx = targetNode._inEdges.indexOf(edge);
    targetNode._inEdges.splice(idx, 1);

    var graphIdx = this._edges.indexOf(edge);
    this._edges.splice(graphIdx, 1);
  }

  delete this._nodes[id];
}

Graph.prototype.addEdge = function(sourceId, targetId, key, customData) {
  var source = this._nodes[sourceId];
  if (source === undefined) {
    throw "source node does not exist: " + sourceId;
  }

  var target = this._nodes[targetId];
  if (target === undefined) {
    throw "target node does not exist: " + targetId;
  }

  for (var i in target._inEdges) {
    if (target._inEdges[i].source.node.id == source.id) {
      // edge already exists; skip
      return;
    }
  }

  if (source._edgeKeys.indexOf(key) == -1) {
    source._edgeKeys.push(key);
  }

  if (target._edgeKeys.indexOf(key) == -1) {
    target._edgeKeys.push(key);
  }

  var edgeSource = source._edgeSources[key];
  if (!edgeSource) {
    edgeSource = new EdgeSource(source, key);
    source._edgeSources[key] = edgeSource;
  }

  var edgeTarget = target._edgeTargets[key];
  if (!edgeTarget) {
    edgeTarget = new EdgeTarget(target, key);
    target._edgeTargets[key] = edgeTarget;
  }

  var edge = new Edge(edgeSource, edgeTarget, key, customData);
  target._inEdges.push(edge);
  source._outEdges.push(edge);
  this._edges.push(edge);
}

Graph.prototype.removeEdge = function(edge) {
  var inIdx = edge.target.node._inEdges.indexOf(edge);
  edge.target.node._inEdges.splice(inIdx, 1);

  var outIdx = edge.source.node._outEdges.indexOf(edge);
  edge.source.node._outEdges.splice(outIdx, 1);

  var graphIdx = this._edges.indexOf(edge);
  this._edges.splice(graphIdx, 1);
}

Graph.prototype.node = function(id) {
  return this._nodes[id];
};

Graph.prototype.nodes = function() {
  var nodes = [];

  for (var id in this._nodes) {
    nodes.push(this._nodes[id]);
  }

  return nodes;
};

Graph.prototype.edges = function() {
  return this._edges;
};

Graph.prototype.layout = function() {
  var rankGroups = [];

  for (var i in this._nodes) {
    var node = this._nodes[i];

    var rankGroupIdx = node.rank();
    var rankGroup = rankGroups[rankGroupIdx];
    if (!rankGroup) {
      rankGroup = new RankGroup(rankGroupIdx);
      rankGroups[rankGroupIdx] = rankGroup;
    }

    rankGroup.nodes.push(node);
  }

  for (var i in this._nodes) {
    var node = this._nodes[i];

    var rankGroup = node.rank();

    var rankGroupOffset = 0;
    for (var c in rankGroups) {
      if (c < rankGroup) {
        rankGroupOffset += rankGroups[c].width() + RANK_GROUP_SPACING;
      }
    }

    node._position.x = rankGroupOffset + ((rankGroups[rankGroup].width() - node.width()) / 2);

    node._edgeKeys.sort(function(a, b) {
      var targetA = node._edgeTargets[a];
      var targetB = node._edgeTargets[b];
      if (targetA && !targetB) {
        return -1;
      } else if (!targetA && targetB) {
        return 1;
      }

      if (targetA && targetB) {
        var introRankA = targetA.rankOfFirstAppearance();
        var introRankB = targetB.rankOfFirstAppearance();
        if(introRankA < introRankB) {
          return -1;
        } else if (introRankA > introRankB) {
          return 1;
        }
      }

      var sourceA = node._edgeSources[a];
      var sourceB = node._edgeSources[b];
      if (sourceA && !sourceB) {
        return -1;
      } else if (!sourceA && sourceB) {
        return 1;
      }

      return compareNames(a, b);
    });
  }

  // first pass: initial rough sorting and layout
  // second pass: detangle now that we know downstream positioning
  for (var repeat = 0; repeat < 2; repeat++) {
    for (var c in rankGroups) {
      rankGroups[c].sortNodes();
      rankGroups[c].layout();
    }
  }

  for (var c in rankGroups) {
    rankGroups[c].groupByAlignment();
    rankGroups[c].layout();
  }

  // for (var repeat = 0; repeat < 11; repeat++) {
  // for (var c in rankGroups) {
  //   if (rankGroups[c].tug()) {
  //     // for (var backwards = c - 1; backwards >= 0; backwards--) {
  //     //   rankGroups[backwards].tug();
  //     // }

  //     anyChanged = true;
  //   }
  // }
  // }

  // var anyChanged = true;
  // var iterations = 0;
  // while (anyChanged) {
  //   anyChanged = false;
  //   for (var c in rankGroups) {
  //     if (rankGroups[c].fixStragglers()) {
  //       anyChanged = true;
  //     }
  //   }

  //   iterations++;

  //   if (iterations == 30) {
  //     alert("bollocks");
  //     break;
  //   }
  // }

//     for (var c in rankGroups) {
//       if (rankGroups[c].fillGaps()) {
//         anyChanged = true;
//       }
//     }
}

Graph.prototype.computeRanks = function() {
  var forwardNodes = {};

  for (var n in this._nodes) {
    var node = this._nodes[n];

    if (node._inEdges.length == 0) {
      node._cachedRank = 0;
      forwardNodes[node.id] = node;
    }
  }

  var bottomNodes = {};

  // walk over all nodes from left to right and determine their rank
  while (!objectIsEmpty(forwardNodes)) {
    var nextNodes = {};

    for (var n in forwardNodes) {
      var node = forwardNodes[n];

      if (node._outEdges.length == 0) {
        bottomNodes[node.id] = node;
      }

      for (var e in node._outEdges) {
        var nextNode = node._outEdges[e].target.node;

        // careful: two edges may go to the same node but be from different
        // ranks, so always destination nodes as far to the right as possible
        nextNode._cachedRank = Math.max(nextNode._cachedRank, node._cachedRank + 1);

        nextNodes[nextNode.id] = nextNode;
      }
    }

    forwardNodes = nextNodes;
  }

  var backwardNodes = bottomNodes;

  // walk over all nodes from right to left and bring upstream nodes as far
  // to the right as possible, so that edges aren't passing through ranks
  while (!objectIsEmpty(backwardNodes)) {
    var prevNodes = {};

    for (var n in backwardNodes) {
      var node = backwardNodes[n];

      // for all upstream nodes, determine latest possible rank group by
      // taking the minimum rank of all downstream nodes and placing it in the
      // rank immediately preceding it
      for (var e in node._inEdges) {
        var prevNode = node._inEdges[e].source.node;

        var latestRank = prevNode.latestPossibleRank();
        if (latestRank !== undefined) {
          prevNode._cachedRank = latestRank;
        }

        prevNodes[prevNode.id] = prevNode;
      }
    }

    backwardNodes = prevNodes;
  }
};

Graph.prototype.collapseEquivalentNodes = function() {
  var nodesByRank = [];

  for (var n in this._nodes) {
    var node = this._nodes[n];

    var byRank = nodesByRank[node.rank()];
    if (byRank === undefined) {
      byRank = {};
      nodesByRank[node.rank()] = byRank;
    }

    if (node.equivalentBy === undefined) {
      continue;
    }

    byEqv = byRank[node.equivalentBy];
    if (byEqv === undefined) {
      byEqv = [];
      byRank[node.equivalentBy] = byEqv;
    }

    byEqv.push(node);
  }

  for (var r in nodesByRank) {
    var byEqv = nodesByRank[r];
    for (var e in byEqv) {
      var nodes = byEqv[e];
      if (nodes.length == 1) {
        continue;
      }

      var chosenOne = nodes[0];
      for (var i = 1; i < nodes.length; i++) {
        var loser = nodes[i];

        for (var ie in loser._inEdges) {
          var edge = loser._inEdges[ie];
          this.addEdge(edge.source.node.id, chosenOne.id, edge.key, edge.customData);
        }

        for (var oe in loser._outEdges) {
          var edge = loser._outEdges[oe];
          this.addEdge(chosenOne.id, edge.target.node.id, edge.key, edge.customData);
        }

        this.removeNode(loser.id);
      }
    }
  }
}

Graph.prototype.addSpacingNodes = function() {
  var edgesToRemove = [];
  for (var e in this._edges) {
    var edge = this._edges[e];
    var delta = edge.target.node.rank() - edge.source.node.rank();
    if (delta > 1) {
      var upstreamNode = edge.source.node;
      var downstreamNode = edge.target.node;

      var repeatedNode;
      var initialCustomData;
      var finalCustomData;
      if (edge.source.node.repeatable) {
        repeatedNode = upstreamNode;
        initialCustomData = null;
        finalCustomData = edge.customData;
      } else {
        repeatedNode = downstreamNode;
        initialCustomData = edge.customData;
        finalCustomData = null;
      }

      for (var i = 0; i < (delta - 1); i++) {
        var spacerID = edge.id() + "-spacing-" + i;

        var spacingNode = this.node(spacerID);
        if (!spacingNode) {
          spacingNode = repeatedNode.copy();
          spacingNode.id = spacerID;
          spacingNode._cachedRank = upstreamNode.rank() + 1;
          this.setNode(spacingNode.id, spacingNode);
        }

        currentCustomData = (i == 0 ? initialCustomData : null)
        this.addEdge(upstreamNode.id, spacingNode.id, edge.key, currentCustomData);

        upstreamNode = spacingNode;
      }

      this.addEdge(upstreamNode.id, edge.target.node.id, edge.key, finalCustomData);

      edgesToRemove.push(edge);
    }
  }

  for (var e in edgesToRemove) {
    this.removeEdge(edgesToRemove[e]);
  }
}

function Ordering() {
  this.spaces = [];
}

Ordering.prototype.fill = function(pos, len) {
  for (var i = pos; i < pos + len; i++) {
    this.spaces[i] = true;
  }
}

Ordering.prototype.free = function(pos, len) {
  for (var i = pos; i < pos + len; i++) {
    this.spaces[i] = false;
  }
}

Ordering.prototype.isFree = function(pos, len) {
  for (var i = pos; i < pos + len; i++) {
    if (this.spaces[i]) {
      return false;
    }
  }

  return true;
}

function RankGroup(idx) {
  this.index = idx;
  this.nodes = [];

  this.ordering = new Ordering();
}

function shuffle (array) {
  var i = 0
    , j = 0
    , temp = null

  for (i = array.length - 1; i > 0; i -= 1) {
    j = Math.floor(Math.random() * (i + 1))
    temp = array[i]
    array[i] = array[j]
    array[j] = temp
  }
}

RankGroup.prototype.sortNodes = function() {
  // shuffle(this.nodes);

  this.nodes.sort(function(a, b) {
    var compare;

    if (a._inEdges.length && b._inEdges.length) {
      // position nodes closer to their upstream sources
      var compare = a.highestUpstreamSource() - b.highestUpstreamSource();
      if (compare != 0) {
        return compare;
      }
    }

    if (a._outEdges.length && b._outEdges.length) {
      // position nodes closer to their downstream targets
      var compare = a.highestDownstreamTarget() - b.highestDownstreamTarget();
      if (compare != 0) {
        return compare;
      }
    }

    if (a._inEdges.length && b._outEdges.length) {
      // position nodes closer to their sources than others that are just
      // closer to their destinations
      var compare = a.highestUpstreamSource() - b.highestDownstreamTarget();
      if (compare != 0) {
        return compare;
      }
    }

    if (a._outEdges.length && b._inEdges.length) {
      // position nodes closer to their sources than others that are just
      // closer to their destinations
      var compare = a.highestDownstreamTarget() - b.highestUpstreamSource();
      if (compare != 0) {
        return compare;
      }
    }

    // place nodes that threaded through upstream nodes higher
    var aPassedThrough = a.passedThroughAnyPreviousNode();
    var bPassedThrough = b.passedThroughAnyPreviousNode();
    if (aPassedThrough && !bPassedThrough) {
      return -1;
    }

    // place nodes that thread through downstream nodes higher
    var aPassesThrough = a.passesThroughAnyNextNode();
    var bPassesThrough = b.passesThroughAnyNextNode();
    if (aPassesThrough && !bPassesThrough) {
      return -1;
    }

    // place nodes with more out edges higher
    var byOutEdges = b._outEdges.length - a._outEdges.length;
    if (byOutEdges != 0) {
      return byOutEdges;
    }

    if (!aPassesThrough && bPassesThrough) {
      return 1;
    }

    // both are of equivalent; compare names so it's at least deterministic

    a.debugMarked = true; // to aid in debugging (adds .marked css class)
    b.debugMarked = true;

    return compareNames(a.name, b.name);
  });
}

Node.prototype.debug = function() {
  return this.groupAlignment();
}

RankGroup.prototype.groupByAlignment = function() {
  var byAlignment = [];

  this.nodes.forEach(function(node) {
    var alignment = node.groupAlignment();
    if (byAlignment[alignment] === undefined) {
      byAlignment[alignment] = [];
    }

    byAlignment[alignment].push(node);

    if (DEBUG[node.id]) {
      console.log("alignment", alignment, byAlignment);
    }
  });

  var newNodes = [];
  for (var a in byAlignment) {
    var group = byAlignment[a];
    if (group === undefined) {
      continue;
    }

    newNodes.push.apply(newNodes, group);
  }

  this.nodes = newNodes;
};


RankGroup.prototype.mark = function() {
  for (var i in this.nodes) {
    this.nodes[i].rankGroupMarked = true;
  }
}

RankGroup.prototype.width = function() {
  var width = 0;

  for (var i in this.nodes) {
    width = Math.max(width, this.nodes[i].width())
  }

  return width;
}

RankGroup.prototype.layout = function() {
  var rollingKeyOffset = 0;

  for (var i in this.nodes) {
    var node = this.nodes[i];

    node._keyOffset = rollingKeyOffset;

    rollingKeyOffset += Math.max(node._edgeKeys.length, 1);
  }

  this.setOrdering();
}

RankGroup.prototype.setOrdering = function() {
  this.ordering = new Ordering();

  for (var i in this.nodes) {
    var node = this.nodes[i];
    this.ordering.fill(node._keyOffset, node._edgeKeys.length);
  }
}

RankGroup.prototype.fillGaps = function() {
  var changed = false;

  this.setOrdering();

  for (var i = 0; i < this.nodes.length; i++) {
    var node = this.nodes[i];

    if (node._inEdges.length !== 0 || node._edgeKeys.length > 1) {
      continue;
    }

    var align = node.outAlignment();
    if (align === undefined) {
      continue;
    }

    if (this.ordering.isFree(align, node._edgeKeys.length)) {
      this.ordering.free(node._keyOffset, node._edgeKeys.length);
      node._keyOffset = align;
      this.ordering.fill(node._keyOffset, node._edgeKeys.length);

      changed = true;
    }
  }

  this.syncNodeOrder();

  return changed;
};

RankGroup.prototype.syncNodeOrder = function() {
  this.nodes.sort(function(a, b) {
    return a._keyOffset - b._keyOffset;
  });
};

RankGroup.prototype.fixStragglers = function() {
  var changed = false;

  this.setOrdering();

  for (var i = 0; i < this.nodes.length; i++) {
    var node = this.nodes[i];

    var inAlign = node.inAlignment();
    var outAlign = node.outAlignment();

    var alignments = [];
    if (inAlign !== undefined) {
      alignments.push(inAlign);
    }

    if (outAlign !== undefined) {
      alignments.push(outAlign);
    }

    if (alignments.length === 0) {
      continue;
    }

    alignments.sort(function(a, b) {
      // i hate javascript
      return a - b;
    });

    var aligned = false;
    for (var a in alignments) {
      var align = alignments[a];
      if (align === node._keyOffset) {
        aligned = true;
        break;
      }

      if (this.moveTo(align, node)) {
        aligned = true;
        changed = true;
        break;
      }
    }

    if (!aligned) {
      for (var a in alignments) {
        var align = alignments[a];
        if (this.moveCloseTo(align, node)) {
          changed = true;
          break;
        }
      }
    }
  }

  this.syncNodeOrder();

  return changed;
};

RankGroup.prototype.moveCloseTo = function(align, node) {
  if (this.moveTo(align, node)) {
    return true;
  } else if (align === node._keyOffset) {
    return false;
  } else {
    return this.moveCloseTo(align + 1, node);
  }
}

RankGroup.prototype.moveTo = function(align, node) {
  if (this.ordering.isFree(align, node._edgeKeys.length)) {
    this.ordering.free(node._keyOffset, node._edgeKeys.length);
    node._keyOffset = align;
    this.ordering.fill(node._keyOffset, node._edgeKeys.length);
    return true;
  }

  return false;
};

RankGroup.prototype.tug = function() {
  var changed = false;

  for (var i = 0; i < this.nodes.length; i++) {
    var node = this.nodes[i];

    var align = node.groupAlignment();
    if (align <= node._keyOffset) {
      continue;
    }

    var delta = align - node._keyOffset;

    node._keyOffset += delta;

    var previousNode = node;
    for (var j = i + 1; j < this.nodes.length; j++) {
      var shiftNode = this.nodes[j];

      var previousBottom = previousNode._keyOffset + previousNode._edgeKeys.length;

      if (shiftNode._keyOffset < previousBottom) {
        shiftNode._keyOffset = previousBottom;
      }

      previousNode = shiftNode;
    }

    changed = true;
  }

  return changed;
}

function Node(opts) {
  // Graph node ID
  this.id = opts.id;
  this.name = opts.name;
  this.class = opts.class;
  this.status = opts.status;
  this.repeatable = opts.repeatable;
  this.key = opts.key;
  this.url = opts.url;
  this.svg = opts.svg;
  this.equivalentBy = opts.equivalentBy;

  // DOM element
  this.label = undefined;

  // [EdgeTarget]
  this._edgeTargets = {};

  // [EdgeSource]
  this._edgeSources = {};

  this._edgeKeys = [];
  this._inEdges = [];
  this._outEdges = [];

  this._cachedRank = -1;
  this._cachedWidth = 0;

  this._keyOffset = 0;

  // position (determined by graph.layout())
  this._position = {
    x: 0,
    y: 0
  };
};

Node.prototype.copy = function() {
  return new Node({
    id: this.id,
    name: this.name,
    class: this.class,
    status: this.status,
    repeatable: this.repeatable,
    key: this.key,
    url: this.url,
    svg: this.svg,
    equivalentBy: this.equivalentBy
  });
};

Node.prototype.width = function() {
  if (this._cachedWidth == 0) {
    var id = this.id;

    var svgNode = this.svg.selectAll("g.node").filter(function(node) {
      return node.id == id;
    })

    var textNode = svgNode.select("text").node();

    if (textNode) {
      this._cachedWidth = textNode.getBBox().width;
    } else {
      return 0;
    }
  }

  return this._cachedWidth + (NODE_PADDING * 2);
}

Node.prototype.height = function() {
  var keys = Math.max(this._edgeKeys.length, 1);
  return (KEY_HEIGHT * keys) + (KEY_SPACING * (keys - 1));
}

Node.prototype.position = function() {
  return {
    x: this._position.x,
    y: (KEY_HEIGHT + KEY_SPACING) * this._keyOffset
  }
}

/* spacing required for firefox to not clip ripple border animation */
Node.prototype.animationRadius = function() {
  if (this.class.search('job') > -1) {
    return 70
  }

  return 0
}

Node.prototype.rank = function() {
  return this._cachedRank;
}

Node.prototype.latestPossibleRank = function() {
  var latestRank;

  for (var o in this._outEdges) {
    var prevTargetNode = this._outEdges[o].target.node;
    var targetPrecedingRank = prevTargetNode.rank() - 1;

    if (latestRank === undefined) {
      latestRank = targetPrecedingRank;
    } else {
      latestRank = Math.min(latestRank, targetPrecedingRank);
    }
  }

  return latestRank;
}

Node.prototype.dependsOn = function(node, stack) {
  for (var i in this._inEdges) {
    var source = this._inEdges[i].source.node;

    if (source == node) {
      return true;
    }

    if (stack.indexOf(this) != -1) {
      continue;
    }

    stack.push(this)

    if (source.dependsOn(node, stack)) {
      return true;
    }
  }

  return false;
}

Node.prototype.highestUpstreamSource = function() {
  var minY;

  var y;
  for (var e in this._inEdges) {
    y = this._inEdges[e].source.effectiveKeyOffset();

    if (minY === undefined || y < minY) {
      minY = y;
    }
  }

  return minY;
};

Node.prototype.highestDownstreamTarget = function() {
  var minY;

  var y;
  for (var e in this._outEdges) {
    y = this._outEdges[e].target.effectiveKeyOffset();

    if (minY === undefined || y < minY) {
      minY = y;
    }
  }

  return minY;
};

Node.prototype.groupAlignment = function() {
  // if (this._inEdges.length === 0 && this._outEdges.length === 1) { // TODO: 1 bit is sketchy
  //   return this._outEdges[0].target.node.groupAlignment();
  // }

  // if (this._inEdges.length === 1 && this._outEdges.length === 0) { // TODO: 1 bit is sketchy
  //   return this._inEdges[0].source.node.groupAlignment();
  // }

  var inAlign = this.inGroupAlignment();
  var outAlign = this.outGroupAlignment();

  if (inAlign === undefined && outAlign === undefined) {
    return this._keyOffset;
  }

  if (inAlign === undefined) {
    return outAlign;
  }

  if (outAlign === undefined) {
    return inAlign;
  }

  return Math.max(inAlign, outAlign);
};

Node.prototype.inGroupAlignment = function() {
  return this.edgeGroupAlignment("_inEdges", "source", "_outEdges", "target");
};

Node.prototype.outGroupAlignment = function() {
  return this.edgeGroupAlignment("_outEdges", "target", "_inEdges", "source");
}

Node.prototype.inAlignment = function() {
  return this.edgeAlignment("_inEdges", "source");
};

Node.prototype.outAlignment = function() {
  return this.edgeAlignment("_outEdges", "target");
}

Node.prototype.edgeGroupAlignment = function(direction, end, oppositeDirection, oppositeEnd) {
  // the lowest node out of the highest node of each rank following the edges

  var highestNodeAtRank, nodeInterestingness;

  for (var e in this[direction]) {
    var edge = this[direction][e];
    var node = edge[end].node;

    var interestingness = 0;

    // if (node._edgeKeys.length > 1) {
    //   // going to a node with more than one key
    //   interestingness++;
    // }

    if (node[direction].length > 0) {
      // going to a node that connects to other nodes
      interestingness++;
    }

    if (interestingness !== undefined && interestingness < nodeInterestingness) {
      // boring; don't align with it
      continue;
    }

    if (highestNodeAtRank === undefined || interestingness > nodeInterestingness || node._keyOffset < highestNodeAtRank._keyOffset) {
      highestNodeAtRank = node;
      nodeInterestingness = interestingness;
    }
  }

  if (highestNodeAtRank === undefined) {
    return undefined;
  }

  // TODO: this seems fishy
  var recurse = highestNodeAtRank.edgeGroupAlignment(direction, end, oppositeDirection, oppositeEnd);
  if (recurse === undefined) {
    if (this[oppositeDirection].length === 0) {
      return highestNodeAtRank._keyOffset;
    } else {
      return Math.max(this._keyOffset, highestNodeAtRank._keyOffset);
    }
  }

  if (this[oppositeDirection].length === 0) {
    return recurse;
  } else {
    return Math.max(this._keyOffset, recurse);
  }
}

Node.prototype.edgeAlignment = function(direction, end) {
  var minAlignment, alignmentInterestingness;

  for (var e in this[direction]) {
    var edge = this[direction][e];
    var alignment = edge[end].effectiveKeyOffset() - this._edgeKeys.indexOf(edge.key);

    var interestingness = 0;

    if (edge[end].node._edgeKeys.length > 1) {
      // going to a node with more than one key
      interestingness++;
    }

    if (this._edgeKeys.indexOf(edge.key) === 0) {
      // prefer aligning with the top of the current node
      interestingness++;
    }

    if (interestingness !== undefined && interestingness < alignmentInterestingness) {
      // boring; don't align with it
      continue;
    }

    if (minAlignment === undefined || interestingness > alignmentInterestingness || alignment < minAlignment) {
      // more interesting, or equally interesting but places the node higher
      minAlignment = alignment;
      alignmentInterestingness = interestingness;
    }
  }

  return minAlignment;
};

Node.prototype.passedThroughAnyPreviousNode = function() {
  for (var e in this._inEdges) {
    var edge = this._inEdges[e];
    if (edge.key in edge.source.node._edgeTargets) {
      return true;
    }
  }

  return false;
};

Node.prototype.passesThroughAnyNextNode = function() {
  for (var e in this._outEdges) {
    var edge = this._outEdges[e];
    if (edge.key in edge.target.node._edgeSources) {
      return true;
    }
  }

  return false;
};

function Edge(source, target, key, customData) {
  this.source = source;
  this.target = target;
  this.key = key;
  this.customData = customData;
}

Edge.prototype.id = function() {
  return this.source.id() + "-to-" + this.target.id();
}

Edge.prototype.bezierPoints = function() {
  var sourcePosition = this.source.position();
  var targetPosition = this.target.position();

  var curvature = 0.5;
  var point2, point3;

  if (sourcePosition.x > targetPosition.x) {
    var belowSourceNode = this.source.node.position().y + this.source.node.height(),
        belowTargetNode = this.target.node.position().y + this.target.node.height();

    point2 = {
      x: sourcePosition.x + 100,
      y: belowSourceNode + 100
    }

    point3 = {
      x: targetPosition.x - 100,
      y: belowTargetNode + 100
    }
  } else {
    var xi = d3.interpolateNumber(sourcePosition.x, targetPosition.x);

    point2 = {
      x: xi(curvature),
      y: sourcePosition.y
    }

    point3 = {
      x: xi(1 - curvature),
      y: targetPosition.y
    }
  }

  var points = [sourcePosition, point2, point3, targetPosition]
  return points
}

Edge.prototype.path = function() {
  points = this.bezierPoints()
  return "M" + points[0].x + "," + points[0].y
       + " C" + points[1].x + "," + points[1].y
       + " " + points[2].x + "," + points[2].y
       + " " + points[3].x + "," + points[3].y;
}

function bezierInterpolate(points, fraction) {
  q = d3.interpolate(points.slice(0, 3), points.slice(1, 4))(fraction)
  r = d3.interpolate(q.slice(0, 2), q.slice(1, 3))(fraction)
  b = d3.interpolate(r[0], r[1])(fraction)
  return b
}

function EdgeSource(node, key) {
  // Node
  this.node = node;

  // Key
  this.key = key;
};

EdgeSource.prototype.width = function() {
  return 0;
}

EdgeSource.prototype.height = function() {
  return 0;
}

EdgeSource.prototype.effectiveKeyOffset = function() {
  return this.node._keyOffset + this.node._edgeKeys.indexOf(this.key);
}

EdgeSource.prototype.id = function() {
  return this.node.id + "-" + this.key + "-source";
}

EdgeSource.prototype.position = function() {
  return {
    x: this.node.position().x + this.node.width(),
    y: (KEY_HEIGHT / 2) + this.effectiveKeyOffset() * (KEY_HEIGHT + KEY_SPACING)
  }
};

function EdgeTarget(node, key) {
  // Node
  this.node = node;

  // Key
  this.key = key;
};

EdgeTarget.prototype.width = function() {
  return 0;
}

EdgeTarget.prototype.height = function() {
  return 0;
}

EdgeTarget.prototype.effectiveKeyOffset = function() {
  return this.node._keyOffset + this.node._edgeKeys.indexOf(this.key);
}

EdgeTarget.prototype.rankOfFirstAppearance = function() {
  if (this._rankOfFirstAppearance !== undefined) {
    return this._rankOfFirstAppearance;
  }

  var inEdges = this.node._inEdges;
  var rank = Infinity;
  for (var i in inEdges) {
    var inEdge = inEdges[i];

    if (inEdge.source.key == this.key) {
      var upstreamNodeInEdges = inEdge.source.node._inEdges;

      if (upstreamNodeInEdges.length == 0) {
        rank = inEdge.source.node.rank();
        break;
      }

      var foundUpstreamInEdge = false;
      for (var j in upstreamNodeInEdges) {
        var upstreamEdge = upstreamNodeInEdges[j];

        if (upstreamEdge.target.key == this.key) {
          foundUpstreamInEdge = true;

          var upstreamRank = upstreamEdge.target.rankOfFirstAppearance()

          if (upstreamRank < rank) {
            rank = upstreamRank;
          }
        }
      }

      if (!foundUpstreamInEdge) {
        rank = inEdge.source.node.rank();
        break;
      }
    }
  }

  this._rankOfFirstAppearance = rank;

  return rank;
}

EdgeTarget.prototype.id = function() {
  return this.node.id + "-" + this.key + "-target";
}

EdgeTarget.prototype.position = function() {
  return {
    x: this.node.position().x,
    y: (KEY_HEIGHT / 2) + this.effectiveKeyOffset() * (KEY_HEIGHT + KEY_SPACING)
  }
};

function compareNames(a, b) {
  var byLength = a.length - b.length;
  if (byLength != 0) {
    // place shorter names higher. pretty arbitrary but looks better.
    return byLength;
  }

  return a.localeCompare(b);
}
