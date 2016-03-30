// (C) Copyright 1997-98, Christopher R. Vincent.  
// All Rights Reserved.
// cvince@mit.edu
// http://mit.edu/cvince/

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Hashtable;
import java.util.Vector;

public class HdirCreator extends Frame implements ActionListener {
  
private static final int MIN_NODE_ARGS = 6;
private static final char STRING_DELIM = '|';
private static final String CREATE = "Create";
private static final String DEST = "Output";
private static final String QUIT = "Quit";
private static final String SOURCE = "Source";

private TextField source_text, dest_text;
private FileDialog dialog;

public HdirCreator() {
  dialog = new FileDialog(this,"Hdir Creator");
  initFrame();
}

public HdirCreator(String source_name, String dest_name) {
  File source = new File(source_name);
  File dest = new File(dest_name);
  compileFile(source,dest);
  System.exit(0);
}

private void initFrame() {
  setBackground(Color.white);
  setForeground(Color.black);

  // set layout
  GridBagLayout layout = new GridBagLayout();
  GridBagConstraints gbc = new GridBagConstraints();
  gbc.fill = GridBagConstraints.HORIZONTAL;
  gbc.insets = new Insets(5,5,5,5);
  gbc.anchor = GridBagConstraints.EAST;
  gbc.weighty = 0.5;
  setLayout(layout);

  // title
  Label label = new Label("Hdir Creator",Label.CENTER);
  gbc.gridwidth = 4;
  layout.setConstraints(label,gbc);
  add(label);

  // source file
  Button button = new Button(SOURCE);
  gbc.gridx = 0;
  gbc.gridy = 1;
  gbc.gridwidth = 1;
  gbc.fill = GridBagConstraints.NONE;
  gbc.weightx = 0.1;
  layout.setConstraints(button,gbc);
  add(button);
  button.addActionListener(this);
  source_text = new TextField();
  source_text.setEditable(false);
  gbc.gridx = 1;
  gbc.gridwidth = GridBagConstraints.REMAINDER;
  gbc.fill = GridBagConstraints.HORIZONTAL;
  gbc.weightx = 0.9;
  layout.setConstraints(source_text,gbc);
  add(source_text);

  // dest file
  button = new Button(DEST);
  gbc.gridx = 0;
  gbc.gridy = 2;
  gbc.gridwidth = 1;
  gbc.fill = GridBagConstraints.NONE;
  gbc.weightx = 0.1;
  layout.setConstraints(button,gbc);
  add(button);
  button.addActionListener(this);
  dest_text = new TextField();
  dest_text.setEditable(false);
  gbc.gridx = 1;
  gbc.gridwidth = GridBagConstraints.REMAINDER;
  gbc.fill = GridBagConstraints.HORIZONTAL;
  gbc.weightx = 0.9;
  layout.setConstraints(dest_text,gbc);
  add(dest_text);

  // create button
  button = new Button(CREATE);
  gbc.gridx = 0;
  gbc.gridy = 3;
  gbc.gridwidth = 1;
  gbc.fill = GridBagConstraints.NONE;
  gbc.weightx = 0.1;
  layout.setConstraints(button,gbc);
  add(button);
  button.addActionListener(this);  

  // quit button
  button = new Button(QUIT);
  gbc.gridx = 3;
  gbc.anchor = GridBagConstraints.EAST;
  gbc.weightx = 0.1;
  layout.setConstraints(button,gbc);
  add(button);
  button.addActionListener(this);
  // size and display main window
  setSize(500,200);
  show();
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

private Node parseNode(String id, Hashtable parse_table) throws Exception {
  String s = (String)parse_table.get(id);
  parse_table.remove(id);
  if (s == null)
    throw new Exception("Node missing or already used in tree: "+id);
  Node n = new Node();
  n.id = id;
  try {
    String[] args = parseCommaList(s);
    if (args.length < MIN_NODE_ARGS) 
      throw new Exception("Too few elements in node "+id+": "+s);
    int arg_idx = 0;
    // display name
    n.name = args[arg_idx];
    arg_idx++;
    // font color
    try {
      if (args[arg_idx].length() == 0)
	n.color_idx = 1;
      else 
	n.color_idx = Integer.parseInt(args[arg_idx]);
    }
    catch (Exception e) {
      throw new Exception("Error parsing font color in node "+id+": "+s); 
    }
    arg_idx++;
    // uri
    n.url = null;
    if (args[arg_idx].length() == 0)
      n.relative_url = null;
    else {
      n.relative_url = args[arg_idx]; 
    }
    arg_idx++;
    // target frame
    if (args[arg_idx].length() == 0)
      n.frame = null;
    else
      n.frame = args[arg_idx];
    arg_idx++;
    // expand state
    if (args[arg_idx].length() == 0)
      n.expand = false;
    if (args[arg_idx].equalsIgnoreCase("yes"))
      n.expand = true;
    else if (args[arg_idx].equalsIgnoreCase("no"))
      n.expand = false;
    else 
      throw new Exception("Illegal expand argument in node "+id+": "+s); 
    arg_idx++;
    // subnodes
    if (args[arg_idx].length() == 0)
      n.sub_nodes = null;
    else {
      n.sub_nodes = new Node[args.length - arg_idx];
      for(int i = 0; i < n.sub_nodes.length; i++) {
	n.sub_nodes[i] = parseNode(args[arg_idx],parse_table);
	arg_idx++;
      }
    }
  }
  catch (Exception e) {
    System.err.println(e.getMessage());
    throw new Exception("Unable to parse node "+id+": "+s); }
  System.err.println(n.id+": "+n.name+", "+n.relative_url);
  return n;
}

private Node[] parseNodes(BufferedReader input) throws Exception {
  String line = input.readLine();
  if (line == null)
    throw new Exception("missing top-level list");
  String[] top_node_ids = parseCommaList(line);
  Hashtable parse_table = new Hashtable();
  while ((line = input.readLine()) != null) {
    String id = line;
    line = input.readLine();
    if (line == null)
      throw new Exception("missing argument list for node named "+id);
    parse_table.put(id.trim(),line);
  }
  Node[] nodes = new Node[top_node_ids.length];
  for(int i = 0; i < nodes.length; i++) 
    nodes[i] = parseNode(top_node_ids[i],parse_table);
  return nodes;
}

private void beep() {
  Toolkit.getDefaultToolkit().beep();
}

private void compileFile(File source, File dest) {
  try {
    BufferedReader input = new BufferedReader(new FileReader(source));
    Node[] nodes = parseNodes(input);
    FileOutputStream output = new FileOutputStream(dest);
    ObjectOutputStream p = new ObjectOutputStream(output);
    p.writeObject(nodes);
    p.flush();
    output.close();
  }
  catch(FileNotFoundException e) {
    beep();
    System.err.println("File not found: "+source.getPath());
    return;
  }
  catch(IOException e) {
    beep();
    System.err.println("I/O error.");
    e.printStackTrace();
    return;
  }
  catch(Exception e) {
    beep();
    System.err.println("Error parsing top-level nodes: "+e.getMessage());
    return;    
  }
  System.err.println("Created file: "+dest.getPath());
}
  
public void actionPerformed(ActionEvent evt) {
  String command = evt.getActionCommand();
  if (command.equals(SOURCE)) {
    dialog.setMode(FileDialog.LOAD);
    dialog.show();
    String dirname = dialog.getDirectory();
    String filename = dialog.getFile();
    if (filename != null)
      source_text.setText((new File(dirname,filename)).getPath());
  }
  else if (command.equals(DEST)) {
    dialog.setMode(FileDialog.SAVE);
    dialog.show();
    String dirname = dialog.getDirectory();
    String filename = dialog.getFile();
    if (filename != null)
      dest_text.setText((new File(dirname,filename)).getPath());
  }
  else if (command.equals(CREATE)) {
    if ((source_text.getText().length() == 0) 
	|| (dest_text.getText().length() == 0)) {
      beep();
      return;
    }
    File source = new File(source_text.getText());
    File dest = new File(dest_text.getText());
    compileFile(source, dest);
  }
  else if (command.equals(QUIT))
    System.exit(0);
}

public static void main(String[] args) {
  HdirCreator app;
  if (args.length == 0) 
    app = new HdirCreator();
  else if (args.length == 2)
    app = new HdirCreator(args[0],args[1]);
  else
    System.err.println("usage: HdirCreator <source-file> <destination-file>");
}

}
