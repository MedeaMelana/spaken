package spaken.model;

public class Intersections {
  
  public static Point intersect(final Line l1, final Line l2) {
    return new Point() {
      public Pos getPos() {
        Pos p1 = l1.getP1().getPos();
        Pos p2 = l1.getP2().getPos();
        Pos p3 = l2.getP1().getPos();
        Pos p4 = l2.getP2().getPos();
        
        // formule van http://en.wikipedia.org/wiki/Line-line_intersection
        // TODO        ^ even permalink van opzoeken
        
        double u = p1.x * p2.y - p1.y * p2.x;
        double v = p3.x * p4.y - p3.y * p4.x;
        double w = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x);
        
        double ax = p3.x - p4.x;
        double bx = p1.x - p2.x;
        double ay = p3.y - p4.y;
        double by = p1.y - p2.y;
        
        return new Pos((u * ax - bx * v) / w, (u * ay - by * v) / w);
      }
    };
  }
  
  public static Point[] intersect(Circle c1, Circle c2) {
    return new Point[2];
  }
	
  public static Point[] intersect(Line l, Circle c) {
    return new Point[2];
  }
  
}
