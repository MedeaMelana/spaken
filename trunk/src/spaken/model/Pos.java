package spaken.model;

/**
 * An object of the <tt>Pos</tt> class represents a fixed position in space.
 */
public class Pos implements Point {
  public final double x, y;
  
  public Pos(double x, double y) {
    this.x = x;
    this.y = y;
  }
  
  public double getX() {
    return x;
  }
  
  public double getY() {
    return y;
  }
  
  public Pos getPos() {
    return this;
  }
  
}
