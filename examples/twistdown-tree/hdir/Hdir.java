// (C) Copyright 1997-98, Christopher R. Vincent.  
// All Rights Reserved.
// cvince@mit.edu
// http://mit.edu/cvince/

import java.applet.Applet;
import java.awt.*;
import java.io.PrintStream;
import java.io.ObjectInputStream;
import java.net.URL;
import java.net.MalformedURLException;
import java.util.Vector;

public class Hdir extends Applet implements Runnable {

private static final int MODE_ERROR = -1;
private static final int MODE_INIT = 0;
private static final int MODE_RUN = 1;  
private static final char STRING_DELIM = '|';
private static final int MARKER_WIDTH = 9;
private static final int MARKER_HEIGHT = 7;
private static final byte[] MARKER_BYTES = {(byte)71,(byte)73,(byte)70,(byte)56,(byte)57,(byte)97,(byte)18,(byte)0,(byte)7,(byte)0,(byte)161,(byte)0,(byte)0,(byte)238,(byte)238,(byte)238,(byte)170,(byte)170,(byte)170,(byte)249,(byte)21,(byte)21,(byte)0,(byte)0,(byte)0,(byte)33,(byte)249,(byte)4,(byte)1,(byte)0,(byte)0,(byte)2,(byte)0,(byte)44,(byte)0,(byte)0,(byte)0,(byte)0,(byte)18,(byte)0,(byte)7,(byte)0,(byte)0,(byte)2,(byte)25,(byte)148,(byte)99,(byte)169,(byte)155,(byte)129,(byte)12,(byte)15,(byte)120,(byte)199,(byte)89,(byte)42,(byte)2,(byte)152,(byte)74,(byte)111,(byte)156,(byte)113,(byte)139,(byte)6,(byte)102,(byte)229,(byte)1,(byte)157,(byte)80,(byte)1,(byte)0,(byte)59};

Thread engine;
Node[] top_nodes, display_nodes;
int app_width, app_height;
Color[] palette;
int bg_color_idx, hilite_color_idx;
int left_padding = 20;
String selected = null;
String start_node;
URL context_url;
Font display_font;        
int y_space = 15;                  
int x_space = 15;                 
Image off_image, marker_image;
Graphics off_graphics;
boolean debug;
int app_mode = MODE_INIT;

public String getAppletInfo() {
  return "(C) 1997-98, Christopher R. Vincent. All Rights Reserved.";
}

private void reportError(String s) {
  if (app_mode != MODE_ERROR) {
    app_mode = MODE_ERROR;
    System.err.println(s);
    repaint();
  }
}

private void reportError(Exception e, String s) {
  if (app_mode != MODE_ERROR) {
    app_mode = MODE_ERROR;
    System.err.println(s);
    e.printStackTrace();
    repaint();
  }
}

private void describeColor(PrintStream ps, Color color) {
  // Print a color hex string for debugging.
  ps.print("#");
  String s = Integer.toHexString(color.getRed());
  if (s.length() == 1)
    ps.print("0");
  ps.print(s);
  s = Integer.toHexString(color.getGreen());
  if (s.length() == 1)
    ps.print("0");
  ps.print(s);
  s = Integer.toHexString(color.getBlue());
  if (s.length() == 1)
    ps.print("0");
  ps.println(s);
}

private String[] parseCommaList(String s) {
  // Parse a comma-separated list of strings.  Leading and
  // trailing whitespace ignored in each item.
  Vector resultVec = new Vector();
  int start,esc;
  int end = -1;
  boolean escape = false;
  while (end < s.length()) {
    start = end+1;
    end = s.indexOf(",",start);
    // check for escape start 
    esc = s.indexOf(STRING_DELIM,start);
    if ((esc != -1) && (esc < end)) {
      escape = true;
      start = esc+1;
      end = s.indexOf(STRING_DELIM,start);
    }
    if (end == -1)
      end = s.length();
    resultVec.addElement(s.substring(start,end).trim());
    if (escape) {  
      escape = false;
      // advance to next comma, break if none
      end = s.indexOf(",",end);
      if (end == -1)
	break;
    } 
  }
  String[] res = new String[resultVec.size()];
  resultVec.copyInto(res);
  return res;
}

private Color parseHexColor(String s) throws Exception {
  // Parse a color in the form of #rrggbb.
  return new Color(Integer.parseInt(s.substring(1), 16));  
}

private Color[] parseColorPalette(String s) throws Exception {
  // If string is null, generate default palette.
  Color[] res;
  if (s == null) {
    res = new Color[2];
    res[0] = Color.white;
    res[1] = Color.black;
  }
  else {
    try {
      String[] els = parseCommaList(s);
      res = new Color[els.length];
      for (int i = 0; i < els.length; i++)
	res[i] = parseHexColor(els[i]);
    }
    catch (Exception e) {
      reportError("Error parsing palette string: "+s); 
      throw e;
    }
  }
  if (debug) {
    System.err.println("Parsed "+res.length+" palette entries:");
    for(int i = 0; i < res.length; i++) { 
      System.err.print("   ");
      describeColor(System.err,res[i]);
    }
  }
  return res;
}

private void loadMarkerImage() {
  marker_image = Toolkit.getDefaultToolkit().createImage(MARKER_BYTES);
  MediaTracker tracker = new MediaTracker(this);
  tracker.addImage(marker_image,0);
  try {
    tracker.waitForID(0);
  }
  catch(InterruptedException e) {}
}

private void parseParameters1() {
  String s;
  // debug
  s = getParameter("debug");
  if (s.equals("yes"))
    debug = true;
  else if (s.equals("no"))
    debug = false;
  else {
    reportError("Illegal debug parameter: "+s);
    return;
  }
  // color palette
  s = getParameter("palette");
  try {
    palette = parseColorPalette(s);
  }
  catch (Exception e) {
    reportError(e,"Error parsing palette parameter: "+s); 
    return;
  }
  // background color 
  s = getParameter("bg_color");
  if (s == null)
    bg_color_idx = 0;
  else {
    try {
      bg_color_idx = Integer.parseInt(s);
    }
    catch (Exception e) {
      reportError("Error parsing background color parameter: "+s);
      return;
    }
  }
  if ((bg_color_idx < 0) || (bg_color_idx >= palette.length)) {
    reportError("Illegal background color palette index: "+s); 
    return; 
  }
  // hilite color 
  s = getParameter("hilite_color");
  if (s == null)
    hilite_color_idx = 1;
  else {
    try {
      hilite_color_idx = Integer.parseInt(s);
    }
    catch (Exception e) {
      reportError("Error parsing hilite color parameter: "+s);
      return;
    }
    if ((hilite_color_idx < 0) || (hilite_color_idx >= palette.length)) {
      reportError("Illegal hilite color palette index: "+s); 
      return; 
    }
  }
}
  
private void parseParameters2() {
  String s;  
  s = getParameter("context_url");
  if (s == null)
    context_url = getDocumentBase();
  else {
    try {
      context_url = new URL(s);
    }
    catch (MalformedURLException e) {
      reportError(e,"Malformed context URL: "+s); 
      return;
    }
  }
  // start node 
  start_node = getParameter("start_node");
  // node hierarchy
  s = getParameter("data_url");
  if (s == null) {
    reportError("No data_url parameter specified.");
    return;
  }
  try {
    URL data_url = new URL(context_url,s);
    ObjectInputStream p = new ObjectInputStream(data_url.openStream());
    top_nodes = (Node[])p.readObject();
    p.close();
  }
  catch(MalformedURLException e) {
    reportError(e,"Malformed data URL: "+s); 
    return;
  }
  catch(Exception e) {
    reportError(e,"Error reading node data.");
  }
  // display font
  s = getParameter("font_size");
  int size = 10;
  if (s != null) {
    try {
      size = Integer.parseInt(s);
      if (size <= 0) {
	reportError("Illegal font size parameter");
	return;
      }
    }
    catch (Exception e) {
      reportError(e,"Error parsing font size parameter: "+s);
      return;
    }
  }
  display_font = new Font("dialog",Font.PLAIN,size);
  // x space
  s = getParameter("x_space");
  if (s != null) {
    try {
      x_space = Integer.parseInt(s);
    }
    catch (Exception e) {
      reportError("Error parsing x space parameter: "+s);
      return;
    }
  }
  // y space
  s = getParameter("y_space");
  if (s != null) {
    try {
      y_space = Integer.parseInt(s);
    }
    catch (Exception e) {
      reportError("Error parsing y space parameter: "+s);
      return;
    }
  }
  left_padding = 2*MARKER_WIDTH;
} 

private int updateDisplay(int display_idx, int depth, Node[] nodes) {
  for(int i = 0; i < nodes.length; i++) {
    if (display_idx >= display_nodes.length)
      break;
    Node n = nodes[i];
    n.x = left_padding + depth * x_space;
    n.y = (display_idx + 1) * y_space;
    display_nodes[display_idx] = n;
    display_idx++;
    if (n.sub_nodes != null) {
      if (n.expand)
	display_idx = updateDisplay(display_idx,depth+1,n.sub_nodes);
    }
  }
  return display_idx;
}

private void updateDisplay() {
  int next = updateDisplay(0,0,top_nodes);
  if (next < display_nodes.length)
    for(int i = next; i < display_nodes.length; i++)
      display_nodes[i] = null;
}
  
public void init() {
  parseParameters1();
  setBackground(palette[bg_color_idx]);
}

public void paint(Graphics g) {
  if (app_mode == MODE_INIT) 
    return;
  if (app_mode == MODE_ERROR) {
    setBackground(Color.black);
    g.setColor(Color.red);
    g.drawString("fatal error",10,20);
    return;
  }
  // off_graphics.setClip(0,0,app_width,app_height);
  off_graphics.setColor(palette[bg_color_idx]);
  off_graphics.fillRect(0,0,MARKER_WIDTH,app_height);
  for(int i = 0; i < display_nodes.length; i++) {
    Node n = display_nodes[i];
    if (n == null)
      break;
    if (n.sub_nodes != null) {
      if (n.expand)
	off_graphics.drawImage(marker_image,-MARKER_WIDTH,n.y-MARKER_HEIGHT,this);
      else
	off_graphics.drawImage(marker_image,0,n.y-MARKER_HEIGHT,this);
    }
  }
  off_graphics.fillRect(MARKER_WIDTH,0,app_width-MARKER_WIDTH,app_height);
  for(int i = 0; i < display_nodes.length; i++) {
    Node n = display_nodes[i];
    if (n == null)
      break;
    if (n.id.equals(selected))
      off_graphics.setColor(palette[hilite_color_idx]);
    else {
      if (n.color_idx >= palette.length) {
	System.err.println("Illegal palette index in node "+n.id+", ignoring.");
	n.color_idx = 0;
      }
      off_graphics.setColor(palette[n.color_idx]);
    }
    off_graphics.setFont(display_font);
    off_graphics.drawString(n.name,n.x,n.y);
  }
  g.drawImage(off_image,0,0,this);
}

public void update(Graphics g) {
  paint(g);
}

private boolean findNode(Node[] nodes, String id) {
  for(int i = 0; i < nodes.length; i++) {
    Node n = nodes[i];
    if (n.id.equals(id)) {
      selectNode(n);
      return true;
    }
    else if ((n.sub_nodes != null) && (findNode(n.sub_nodes,id))) {
      n.expand = true;
      return true;
    }
  }
  return false;
}

// Call this method from JavaScript or equivalent to update the
// directory state.
public void selectNode(String id) {
  // Select a node according to its id string.  Perform a depth-first
  // search.
  if (!findNode(top_nodes,id)) 
    // don't go into error mode
    System.err.println("Node "+id+" not found.");
  updateDisplay();
  repaint();
}

private void showDocument(URL u, String f) {
  if (debug) {
    System.err.println("Show "+u.toExternalForm());
    if (f != null)
      System.err.println("   Frame: "+f);
  }
  if (f == null)
    getAppletContext().showDocument(u,"_self");
  else
    getAppletContext().showDocument(u,f);
}

private boolean selectFirstNode(Node[] nodes) {
  // Find and display the first node with a URL, make it visible.
  for(int i = 0; i < nodes.length; i++) {
    Node n = nodes[i];
    if (n.relative_url != null) {
      selectNode(n);     // as if user directly selected this node
      return true;
    }
    else if ((n.sub_nodes != null) && (selectFirstNode(n.sub_nodes))) {
      n.expand = true;
      return true;
    }
  }
  return false;
}

private void selectNode(Node n) {
  // Perform action for a user directly selecting a node.
  n.expand = true;
  // Merge URL, delete if malformed.
  if ((n.url == null) && (n.relative_url != null)) {
    try {
      n.url = new URL(context_url,n.relative_url);
    }
    catch(MalformedURLException e) {
      System.err.println("Malformed URL in node "+n.id+", ignoring.");
      n.relative_url = null;
    }
  }
  if (n.url != null) {
    selected = n.id;
    showDocument(n.url,n.frame);
  }
  else if (n.sub_nodes != null) 
    selectFirstNode(n.sub_nodes);
}

private int getDisplayIndex(int y) {
  // Translate mouse y coordinate to display_nodes index.
  return (y - y_space/2)/y_space;
}

public boolean mouseDown(java.awt.Event evt, int x, int y) {
  if (app_mode != MODE_RUN)
    return true;
  int idx = getDisplayIndex(y);
  Node n = display_nodes[idx];
  if (n == null) 
    return true;
  if (x < n.x) {
    if (n.sub_nodes != null) {
      n.expand = ! n.expand;
      updateDisplay();
      repaint();
    }
    return true;
  }
  else {
    selectNode(n);
    updateDisplay();
    repaint();
  }
  return true;
}

public boolean mouseMove(java.awt.Event evt, int x, int y) {
  if (app_mode != MODE_RUN)
    return true;
  Node n = display_nodes[getDisplayIndex(y)];
  if ((n == null) || (x < n.x) || (n.relative_url == null))
    showStatus(null);
  else
    showStatus(n.relative_url);
  return true;
}

public boolean mouseExit(java.awt.Event evt, int x, int y) {
  if (app_mode != MODE_RUN)
    return true;
  showStatus(null);
  return true;
}

public void start() {
  if (engine == null) {
    engine = new Thread(this);
    engine.start(); }
}

public void stop() {
  if (engine != null && engine.isAlive()) {
    engine.stop(); }
  engine = null;
}
  
public void run() {
  Thread me = Thread.currentThread();
  try {
    Dimension d = size();
    app_width = d.width;
    app_height = d.height;
    parseParameters2();
    loadMarkerImage();
    off_image = createImage(app_width,app_height);
    off_graphics = off_image.getGraphics();
    display_nodes = new Node[app_height/y_space + 1];
    if (start_node != null)
      selectNode(start_node);
    updateDisplay();
    app_mode = MODE_RUN;
    repaint();
  }
  catch (Exception e) {
    reportError("Error during initialization");
  }
}

}
