// (C) Copyright 1997-98, Christopher R. Vincent.  
// All Rights Reserved.
// cvince@mit.edu
// http://mit.edu/cvince/

import java.io.Serializable;
import java.net.URL;

public class Node implements Serializable {

public String id;
public String name;
public int color_idx;
public URL url;
public String relative_url;
public String frame;
public boolean expand;
public Node[] sub_nodes;
public int x, y;

public Node() {}

}
