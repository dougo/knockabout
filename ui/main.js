svgNS = "http://www.w3.org/2000/svg";
xlinkNS = "http://www.w3.org/1999/xlink";
PaleGreen3 = "#7CCD7C";
PaleGreen4 = "#548B54";
SQRT2 = Math.sqrt(2);
SQRT3 = Math.sqrt(3);

// The unit hexagon is a regular hexagon centered at the origin with
// the flat side up, circumscribed by the unit circle: width = 2,
// height = sqrt(3).
hexPoints =
  "-1,0 -0.5," + SQRT3/2 + " 0.5," + SQRT3/2 +
  " 1,0 0.5," + -SQRT3/2 + " -0.5," + -SQRT3/2;

// Die shapes that are circumscribed by the unit hexagon, leaving room
// for 0.1 stroke-width.
trianglePoints =
  "0," + (-SQRT3/2+0.1) + " 0.7," + (SQRT3/4-0.05) + " -0.7, " + (SQRT3/4-0.05);
squareInradius = SQRT3/SQRT2/2-0.05;
squarePoints =
  -squareInradius + "," + -squareInradius + " " + 
   squareInradius + "," + -squareInradius + " " +
   squareInradius + "," +  squareInradius + " " +
  -squareInradius + "," +  squareInradius;
diamondPoints =
  "0," + (-SQRT3/2+0.1) + " " + "0.5,0 " +
  "0," + (SQRT3/2-0.1) + " " + "-0.5,0";

// The shape of an arrow pointing through the north edge of a hex,
// height = sqrt(3)/2, width = 1.
arrowPoints =
  "0," + -SQRT3*7/8 +
  " 0.5," + -SQRT3*5/8 + " 0.25," + -SQRT3*5/8 +
  " 0.25," + -SQRT3*3/8 + " -0.25," + -SQRT3*3/8 +
  " -0.25," + -SQRT3*5/8 + " -0.5," + -SQRT3*5/8;

function makeGroup() {
  return document.createElementNS(svgNS, "g");
}

function makeHex() {
  var hex = document.createElementNS(svgNS, "polygon");
  hex.setAttribute("points", hexPoints);
  return hex;
}

function moveXY(node, x, y) {
  node.setAttribute("transform", "translate(" + x + "," + y + ")");
  return node;
}

// Convert board (row,col) coordinates into (x,y) coordinates.
// The center of the board is at the origin, i.e. (6,6) -> (0,0).
function moveRC(node, row, col) {
  col -= 6; row -= 6;
  return moveXY(node, 1.5*col, SQRT3/2*(2*row - col));
}

function isGutterRC(row, col) {
  return (row == 0 || row == 12 || col == 0 || col == 12 ||
	  row-col == 6 || row-col == -6);
}

function makeBoard() {
  var board = makeGroup();
  moveXY(board, 12, 12);
  var gutter = makeHex();
  gutter.setAttribute("transform", "rotate(30) scale(" + 6*SQRT3 + ")");
  gutter.setAttribute("stroke", PaleGreen4);
  gutter.setAttribute("stroke-width", 0.04);
  gutter.setAttribute("fill-opacity", 0);
  board.appendChild(gutter);

  for (var col = 0; col < 13; col++) {
    for (var row = Math.max(0, col - 6); row < Math.min(col + 7, 13); row++) {
      var hex = makeHex();
      hex.setAttribute("fill", PaleGreen3);
      hex.setAttribute("stroke", "black");
      hex.setAttribute("stroke-width", 0.075);
      moveRC(hex, row, col);
      if (isGutterRC(row, col))
	// Gutter space, shrink to half size.
	hex.setAttribute("transform", hex.getAttribute("transform")
			 + " scale(0.5)");
      board.appendChild(hex);
    }
  }

  return board;
}

function dieShapePoints(size) {
  switch (size) {
  case 4: return trianglePoints;
  case 6: return squarePoints;
  case 8: return diamondPoints;
  }
}

function makeDieShape(owner, size) {
  var shape = document.createElementNS(svgNS, "polygon");
  shape.setAttribute("points", dieShapePoints(size));
  shape.setAttribute("stroke", "black");
  // TO DO: let user choose colors
  if (owner == 0) {
    shape.setAttribute("fill", "gold");
  }
  shape.setAttribute("stroke-width", 0.05);
  return shape;
}

function makeDieText(owner, face) {
  var text = document.createElementNS(svgNS, "text");
  text.setAttribute("text-anchor", "middle");
  text.setAttribute("font-family", "sans-serif");
  text.setAttribute("font-weight", "bold");
  text.setAttribute("font-size", 0.7);
  text.setAttribute("y", 0.2);
  // TO DO: figure out text color from background color,
  // i.e. light on dark or dark on light
  if (owner == 1) text.setAttribute("fill", "white");
  text.textContent = face;
  return text;
}

function makeArrow(group, dir) {
  var arrow = document.createElementNS(svgNS, "polygon");
  arrow.setAttribute("points", arrowPoints);
  arrow.setAttribute("stroke", "black");
  arrow.setAttribute("fill", "red");
  arrow.setAttribute("stroke-width", 0.05);
  arrow.setAttribute("transform", "rotate(" + dir*60 + ")");
  arrow.setAttribute("onactivate", "moveDie(" + dir + ")");
  return group.appendChild(arrow);
}

function makeArrows() {
  var arrows = makeGroup();
  for (var dir = 0; dir < 6; dir++)
    makeArrow(arrows, dir);
  return arrows;
}

function activateDie(row, col) {
  activatedPosition = [row, col];
  // TO DO: in the gutter, don't show all 6 arrows!
  moveRC(arrows, row, col);
  board.appendChild(arrows);
}

function moveDie(dir) {
  rpc("move", activatedPosition, dir);
  board.removeChild(arrows);
}

root = document.documentElement;
board = root.appendChild(makeBoard());
arrows = makeArrows();
dice = null;

/**
 * The configs parameter is a struct; each property name is a seat
 * name and its value is a seat configuration.
 * A seat configuration is an array of dice configurations.
 * A dice configuration is an array of structs; each struct has the
 * properties row, col, size, face.
 */
game.setDice = function(configs) {
  if (dice) board.removeChild(dice);
  dice = makeGroup();
  for (var seatName in configs) {
    var seatConfig = configs[seatName];
    for (var dieNum in seatConfig) {
      var dieConfig = seatConfig[dieNum];
      var die = document.createElementNS(svgNS, "g");
      die.appendChild(makeDieShape(seatName, dieConfig.size));
      die.appendChild(makeDieText(seatName, dieConfig.face));
      moveRC(die, dieConfig.row, dieConfig.col);
      die.setAttribute("onactivate",
		       "activateDie(" +
		       dieConfig.row + "," +
		       dieConfig.col + ")");
      dice.appendChild(die);
    }
  }
  board.appendChild(dice);
  return true;
}
