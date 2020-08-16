pico-8 cartridge // http://www.pico-8.com
version 29
__lua__

------------------------------------
-- Base Objects
------------------------------------

-- creates a new object by calling obj = object:extend()
object={}
function object:extend(kob)
  kob=kob or {}
  kob.extends=self
  return setmetatable(kob,{
   __index=self,
   __call=function(self,ob)
	   ob=setmetatable(ob or {},{__index=kob})
	   local ko,init_fn=kob
	   while ko do
	    if ko.init and ko.init~=init_fn then
	     init_fn=ko.init
	     init_fn(ob)
	    end
	    ko=ko.extends
	   end
	   return ob
  	end
  })
end

vector={}
vector.__index=vector
 -- operators: +, -, *, /
 function vector:__add(b)
  return v(self.x+b.x,self.y+b.y)
 end
 function vector:__sub(b)
  return v(self.x-b.x,self.y-b.y)
 end
 function vector:__mul(m)
  return v(self.x*m,self.y*m)
 end
 function vector:__div(d)
  return v(self.x/d,self.y/d)
 end
 function vector:__unm()
  return v(-self.x,-self.y)
 end
function vector:__neq(v)
  return not (self.x==v.x and self.y==v.y)
end
function vector:__eq(v)
  return self.x==v.x and self.y==v.y
end
 -- dot product
 function vector:dot(v2)
  return self.x*v2.x+self.y*v2.y
 end
 -- normalization
 function vector:norm()
  return self/sqrt(#self)
 end
 -- length
 function vector:len()
  return sqrt(#self)
 end
 -- the # operator returns
 -- length squared since
 -- that's easier to calculate
 function vector:__len()
  return self.x^2+self.y^2
 end
 -- printable string
 function vector:str()
  return self.x..","..self.y
 end

-- creates a new vector with
-- the x,y coords specified
function v(x,y)
 return setmetatable({
  x=x,y=y
 },vector)
end



entity=object:extend(
  {
    t=0,
    spawns={}
  }
)

 -- common initialization
 -- for all entity types
function entity:init()  
  if self.sprite then
   self.sprite=deep_copy(self.sprite)
   if not self.render then
    self.render=spr_render
   end
  end
end
 -- called to transition to
 -- a new state - has no effect
 -- if the entity was in that
 -- state already
function entity:become(state)
  if state~=self.state then
   self.state,self.t=state,0
  end
end
-- checks if entity has 'tag'
-- on its list of tags
function entity:is_a(tag)
  if (not self.tags) return false
  for i=1,#self.tags do
   if (self.tags[i]==tag) return true
  end
  return false
end
 -- called when declaring an
 -- entity class to make it
 -- spawn whenever a tile
 -- with a given number is
 -- encountered on the level map
function entity:spawns_from(...)
  for tile in all({...}) do
   entity.spawns[tile]=self
  end
end

------------------------------------
-- dynamic objects
------------------------------------

dynamic=entity:extend({
    maxvel=1,
    acc=0.5,
    fric=0.5,
    vel=v(0,0),
    dir=v(0,0)
  })

------------------------------------
-- collision boxes
------------------------------------

-- collision boxes are just
-- axis-aligned rectangles
cbox=object:extend()
 -- moves the box by the
 -- vector v and returns
 -- the result
 function cbox:translate(v)
  return cbox({
   xl=self.xl+v.x,
   yt=self.yt+v.y,
   xr=self.xr+v.x,
   yb=self.yb+v.y
  })
 end

 -- checks if two boxes
 -- overlap
 function cbox:overlaps(b)
  return
   self.xr>b.xl and
   b.xr>self.xl and
   self.yb>b.yt and
   b.yb>self.yt
 end

 -- calculates a vector that
 -- neatly separates this box
 -- from another. optionally
 -- takes a table of allowed
 -- directions
function cbox:sepv(b,allowed)
  local candidates={
    v(b.xl-self.xr,0),
    v(b.xr-self.xl,0),
    v(0,b.yt-self.yb),
    v(0,b.yb-self.yt)
  }
  if type(allowed)~="table" then
   allowed={true,true,true,true}
  end
  local ml,mv=32767
  for d,v in pairs(candidates) do
   if allowed[d] and #v<ml then
    ml,mv=#v,v
   end
  end

  return mv
end
 
 -- printable representation
 function cbox:str()
  return self.xl..","..self.yt..":"..self.xr..","..self.yb
 end

-- makes a new box
function box(xl,yt,xr,yb) 
 return cbox({
  xl=min(xl,xr),xr=max(xl,xr),
  yt=min(yt,yb),yb=max(yt,yb)
 })
end


------------------------------------
-- particles
--    common class for all
--    particles
-- Implements init(), update() and
-- render()
------------------------------------

particle=object:extend(
  {
    t=0,vel=v(0,0),
    lifetime=30
  }
)

------------------------------------
-- Bucket / Collisions
------------------------------------

c_bucket = {}

function do_movement()
  for e in all(entities) do
      if e.vel then
        for i=1,2 do
          e.pos.x+=e.vel.x/2
          collide_tile(e)
          
          e.pos.y+=e.vel.y/2
          collide_tile(e)
        end
      end
    end
end

function bkt_pos(e)
  local x,y=e.pos.x,e.pos.y
  return flr(shr(x,4)),flr(shr(y,4))
end

-- add entity to all the indexes
-- it belongs in the bucket
function bkt_insert(e)
  local x,y=bkt_pos(e)
  for t in all(e.tags) do
    local b=bkt_get(t,x,y)
    add(b,e)
  end

  e.bkt=v(x,y)
end

function bkt_remove(e)
  local x,y=e.bkt.x,e.bkt.y
  for t in all(e.tags) do
    local b=bkt_get(t,x,y)
    del(b,e)
  end
end

function bkt_get(t,x,y)
  local ind=t..":"..x..","..y
  if not c_bucket[ind] then
    c_bucket[ind]={}
  end
  return c_bucket[ind]
end

function bkt_update()  
  for e in all(entities) do
    bkt_update_entity(e)
  end
end

function bkt_update_entity(e)
  if not e.pos or not e.tags then return end
  local bx,by=bkt_pos(e)
  if not e.bkt or e.bkt.x~=bx or e.bkt.x~=by then
    if not e.bkt then
      bkt_insert(e)
    else
      bkt_remove(e)
      bkt_insert(e)
    end
  end
end

-- iterator that goes over
-- all entities with tag "tag"
-- that can potentially collide
-- with "e" - uses the bucket
-- structure described earlier.
function c_potentials(e,tag)
 local cx,cy=bkt_pos(e)
 local bx,by=cx-2,cy-1
 local bkt,nbkt,bi={},0,1
 return function()
  -- ran out of current bucket,
  -- find next non-empty one
  while bi>nbkt do
   bx+=1
   if (bx>cx+1) bx,by=cx-1,by+1
   if (by>cy+1) return nil
   bkt=bkt_get(tag,bx,by)
   nbkt,bi=#bkt,1
  end
  -- return next entity in
  -- current bucket and
  -- increment index
  local e=bkt[bi]
  bi+=1
  return e
 end 
end

function do_collisions()    
  	for e in all(entities) do
      collide(e)
    end
end

function collide(e)
  if not e.collides_with then return end
  if not e.hitbox then return end

  local ec=c_get_entity(e)

  ---------------------
  -- entity collision
  ---------------------
  for tag in all(e.collides_with) do
    --local bc=bkt_get(tag,e.bkt.x,e.bkt.y)
    for o in  c_potentials(e,tag) do  --all(entities[tag]) do
      -- create an object that holds the entity
      -- and the hitbox in the right position
      local oc=c_get_entity(o)
      -- call collide function on the entity
      -- that e collided with
      if o~=e and ec.b:overlaps(oc.b) then
        if ec.e.collide then 
          local func,arg=ec.e:collide(oc.e)
          if func then
            func(ec,oc,arg)            
          end
        end
      end

    end
  end
end

------------------------------------
-- Tile Collision
------------------------------------

function collide_tile(e)  
  -- do not collide if it's not set to
  if (not e.c_tile) return

  local ec=c_get_entity(e)

  local pos=tile_flag_at(ec.b, 1)

  for p in all(pos) do
    local oc={}
    oc.b=box(p.x,p.y,p.x+8,p.y+8)

    -- only allow pushing to empty spaces
    local dirs={v(-1,0),v(1,0),v(0,-1),v(0,1)}
    local allowed={}
    for i=1,4 do
      local np=v(p.x/8,p.y/8)+dirs[i]
      allowed[i]= not is_solid(np.x,np.y) and not (np.x < 0 or np.x > 127 or np.y < 0 or np.y > 63)
    end

    c_push_out(oc, ec, allowed)
    if (ec.e.tcollide) ec.e:tcollide()    
  end
end

-- get entity with the right position
-- for cboxes
function c_get_entity(e)
  local ec={e=e,b=e.hitbox}
  if (ec.b) ec.b=ec.b:translate(e.pos)
  return ec
end

function tile_at(cel_x, cel_y)
	return mget(cel_x, cel_y)
end

function is_solid(cel_x,cel_y)
  return fget(mget(cel_x, cel_y),1)
end

function solid_at(x, y, w, h)
	return #tile_flag_at(box(x,y,x+w,y+h), 1)>0
end

function tile_flag_at(b, flag)
  local pos={}

	for i=flr(b.xl/8), ((ceil(b.xr)-1)/8) do
		for j=flr(b.yt/8), ((ceil(b.yb)-1)/8) do
			if(fget(tile_at(i, j), flag)) then
				add(pos,{x=i*8,y=j*8})
			end
		end
	end

  return pos
end

-- reaction function, used by
-- returning it from :collide().
-- cause the other object to
-- be pushed out so it no
-- longer collides.
function c_push_out(oc,ec,allowed_dirs)
 local sepv=ec.b:sepv(oc.b,allowed_dirs)
 if not sepv then return end
 ec.e.pos+=sepv
 if ec.e.vel then
  local vdot=ec.e.vel:dot(sepv)
  if vdot<0 then   
   if sepv.x~=0 then ec.e.vel.x=0 end
   if sepv.y~=0 then ec.e.vel.y=0 end
  end
 end
 ec.b=ec.b:translate(sepv)
 return sepv
 end
-- inverse of c_push_out - moves
-- the object with the :collide()
-- method out of the other object.
-- function c_move_out(oc,ec,allowed)
--  return c_push_out(ec,oc,allowed)
-- end

------------------------------------
-- Entity Handling
------------------------------------

entities = {}
particles = {}
r_entities = {}

function update_draw_order(e, ndr)
    e.draw_order = e.draw_order or 3
    del(r_entities[e.draw_order], e)

    e.draw_order = ndr or 3
    add(r_entities[e.draw_order], e)
end

function p_add(p)  
  add(particles, p)
end

function p_remove(p)
  del(particles, p)
end

function p_update()
  for p in all(particles) do
    if p.pos and p.vel then
      p.pos+=p.vel
    end
    if (p.update) p:update()

    if (p.t > p.lifetime or p.done)p_remove(p)
    p.t+=1
  end
end

-- adds entity to all entries
-- of the table indexed by it's tags
function e_add(e)
  add(entities, e)

  local dr=e.draw_order or 3
  if (not r_entities[dr]) r_entities[dr]={}
  add(r_entities[dr],e)
end

function e_remove(e)
  del(entities, e)
  for tag in all(e.tags) do        
    if e.bkt then
      del(bkt_get(tag, e.bkt.x, e.bkt.y), e)
    end
  end

  del(r_entities[e.draw_order or 3],e)

  if e.destroy then e:destroy() end
end

-- loop through all entities and
-- update them based on their state
function e_update_all()  
  for e in all(entities) do
    if (e[e.state])e[e.state](e)
    if (e.update)e:update()
    e.t+=1

    if e.done then
      e_remove(e)
    end
  end
end

function e_draw_all()
  for i=0,7 do
    for e in all(r_entities[i]) do
      if (e.render)e:render()
    end
  end
end

function p_draw_all()
  for p in all(particles) do
    p:render()
  end
end

function spr_render(e)
  spr(e.sprite, e.pos.x, e.pos.y)
end

invoke_func = {}
function invoke(func,t,p)
  add(invoke_func,{func,0,t,p})
end

function update_invoke()
  for i=#invoke_func,1,-1 do
    invoke_func[i][2]+=1
    if invoke_func[i][2]>=invoke_func[i][3] then
      invoke_func[i][1](invoke_func[i][4])
      del(invoke_func,invoke_func[i])
    end
  end
end

------------------------------------
-- Utils
------------------------------------

function is_close(v1,v2,e)
		if(type(v1)~="number")return (v1-v2):len()<e
		return abs(v1-v2)<e
end

function sign(val)
  return val<0 and -1 or (val > 0 and 1 or 0)
end

function frac(val)
  return val-flr(val)
end

function ceil(val)
  if (frac(val)>0) return flr(val+sign(val)*1)
  return val
end

function approach(val,target,step)
  step=abs(step)
  return val < target and min(val+step,target) or max(val-step,target)
end

function clamp(low, hi, val)
	return (val < low) and low or (val > hi and hi or val)
end

function choose(arg)
    return arg[flr(rnd(#arg)) + 1]
end

function e_is_any(e, op)
  for i in all(e.tags) do
    for o in all(op) do
      if e:is_a(o) then return true end
    end
  end

  return false
end

function deep_copy(obj)
 if (type(obj)~="table") return obj
 local cpy={}
 setmetatable(cpy,getmetatable(obj))
 for k,v in pairs(obj) do
  cpy[k]=deep_copy(v)
 end
 return cpy
end

function shallow_copy(obj)
  if (type(obj)~="table") return obj
 local cpy={} 
 for k,v in pairs(obj) do
  cpy[k]=v
 end
 return cpy
end

------------------------------------
-- Boilerplate Code
------------------------------------

state = nil
palt(0, true)

-- Destroys everything from current state
function reset_state()
	entities = {}
	particles = {}
	r_entities = {}
end

function change_state(new_state,...)
    state = new_state

    -- Destroy everything
    reset_state()

    state:init(...)
end

function _init()
    change_state(initialstate)
end

function _update()
    state:update()
end

function _draw()
    state:draw()
end

--------------------------------------------------------------------------------------------
-- actual game code
--------------------------------------------------------------------------------------------

screen_size = 64
poke(0x5f2c, 3)
palt(0,false)
palt(1,true)

------------------------------------
-- baseState
------------------------------------

basestate = object:extend{}

function basestate:init()

end

function basestate:update()
    e_update_all()
    bkt_update()
    do_movement()
    do_collisions()
    p_update()
    update_invoke()
end

function basestate:draw()
    cls()

    e_draw_all()
    p_draw_all()

    handle_camera()
end

------------------------------------
-- menu State
------------------------------------

menustate = basestate:extend({})

function menustate:init()
				e_add(menu)
end

------------------------------------
-- dead state
------------------------------------

deadstate=basestate:extend({})

function deadstate:init(...)
	local arg={...}
	self.wave=arg[1]
	self.time=arg[2]
end

function deadstate:update()
	if btn(5) then run() end
end

function deadstate:draw()
	cls()
	
	local survived="waves: " .. tostr(self.wave)
	local timeplayed="time played: " .. tostr(self.time/30) .. "s"
	print(
		survived,
		(64-#survived*4)/2,28,7
		)
	print(
		timeplayed,
		(64-#timeplayed*4)/2,34,7
		)
end

------------------------------------
-- game State
------------------------------------

gamestate = basestate:extend({})

function gamestate:init()
				e_add(arena)
				
				p = player{pos=v(screen_size / 2, screen_size / 2)}
    e_add(p)
    
    s=spawner{player=p}
    p.spawner=s
    
				e_add(s) 
				
				e_add(tutorial{player=p,spawner=s})   
end

function gamestate:render()
	map(0,0,0,0,8,8)
end

-------------------------------
-- begin game
-------------------------------

initialstate=menustate

-------------------------------
-- event
-------------------------------

event = object:extend{
	origin=nil,
	call=nil
}

function event:exec()
	self.call(self.origin)
end

-------------------------------
-- camera functions
-------------------------------

shake=0
s_amount=1
function handle_camera()
  camera()

  if shake > 0 then 
    shake-=1 
    camera(rnd(s_amount)-s_amount/2, rnd(s_amount)-s_amount/2)
  end
end

-------------------------------
-- entity: menu
-------------------------------

menu=entity:extend({})

function menu:update()
		if btn(5) then
				change_state(gamestate)
		end
end

function menu:render()
		map(8,8,0,0,8,8)
		print("press x to start", 0, screen_size/2)
end

-------------------------------
-- entity: arena
-------------------------------

arena=entity:extend({})

function arena:render()
		map(0,0,0,0,8,8)
end

-------------------------------
-- entity: tutorial
-------------------------------

tutorial=entity:extend({})

function tutorial:init()
		self:become("changingdirection")
		self.spawner.stopchange=true
		self.spawner.ignore_wave_start=true
end

function tutorial:leveling()
		if self.player.level~=1 then
				self:become("ending")
		end
end

function tutorial:changingdirection()
		self.showtext=true
		if self.player.a~=0 then
				self:become("leveling")
				invoke(function() self.spawner:nextwave() end, 30, self)
		end
end

function tutorial:ending()
		invoke(function()
				self.player:become("walking")
				self.spawner.stopchange=false
				self.spawner.ignore_wave_start=false
				self.done=true
		 end, 30, self)
end

function tutorial:render()
		if self.showtext then
			print("x+arrows:",
					15,screen_size*2/3, 7)
			print("change direction",
					0,screen_size*5/6, 7)
		end
end

-------------------------------
-- entity: explosion particle
-------------------------------

-------------------------------
-- smoke particle
-------------------------------

smoke=particle:extend(
  {
    vel=v(0,0),
    c=9,
    v=0.1
  }
)

function smoke:init()
  self.vel=v(rnd(0.5)-0.25,-(rnd(1)+0.5))
  if (not self.r) self.r=rnd(1)+0.5
end

function smoke:update()
  self.r-=self.v
  if (self.r<=0) self.done=true
end

function smoke:render()
  if (not self.pos) return  
  circfill(self.pos.x, self.pos.y, self.r, self.c)
end

function explode(x, y, w, h)
  for i=0,10 do
    p_add(smoke{pos=v(x+w/2+rnd(1)-0.5, 
                      y+h/2+rnd(1)-0.5),
                c=choose({7,8,9}),r=rnd(2)+1,v=rnd(0.2)+0.2})
  end
end

-------------------------------
-- entity: dust particle
-------------------------------

dust=particle:extend({ })

-- adapted from: https://www.lexaloffle.com/bbs/?pid=58210
function add_new_dust(x,y,dx,dy,l,s,g,p,f)
	p_add(dust{
	    fade=f,
	    pos=v(x,y),
	    dx=dx,
	    dy=dy,
	    lifetime=l,
	    rad=s,
	    p=p,
	    col=0, --set to color
	    grav=g
	})
end

function dust:update() 
 --move the particle based on
 --the speed
 self.pos.x+=self.dx
 self.pos.y+=self.dy
 --and gravity
 self.dy+=self.grav

 --reduce the radius
 --this is set to 90%, but
 --could be altered
 self.rad*=self.p

 --set the color
 if type(self.fade)=="table" then
  --assign color from fade
  --this code works out how
  --far through the lifespan
  --the particle is and then
  --selects the color from the
  --table
  self.col=self.fade[flr(#self.fade*(self.t/self.lifetime))+1]
 else
  --just use a fixed color
  self.col=self.fade            
 end
end

function dust:render()
 circfill(self.pos.x,self.pos.y,self.rad,self.col)
end

-------------------------------
-- entity: turnable
-------------------------------

turnable=dynamic:extend({
    a=0,
   	size=1
})

function turnable:render() 
		self:renderrot(self.sprite, self.pos-v(self.size,self.size),
		self.a, self.size, self.inverted, self.ignore)
end

function turnable:renderrot(sprite, pos, a, size, invert, ignore) 
		if (not ignore or
					ignore==self.ignore) and
					a%180==0 then
				invertxor=(a~=180 and invert) or (a==180 and not invert)
				spr(sprite,pos.x,pos.y,size,size,invertxor, a==180)
		else
		  self:rspr(8*sprite%128,
		  	flr(sprite/16)*8,
		  	pos.x,pos.y,
		  	a/360,size,invert,ignore)
		end
end

function turnable:rspr(sx,sy,x,y,a,w,invert,ignore)
  local ca,sa=cos(a),sin(a)
  local srcx,srcy
  local ddx0,ddy0=ca,sa
  local mask=shl(0xfff8,(w-1))
  w*=4
  ca*=w-0.5
  sa*=w-0.5
  local dx0,dy0=sa-ca+w,-ca-sa+w
  w=2*w-1
  r=r or w

  for ix=0,w do
      srcx,srcy=dx0,dy0
      for iy=0,w do
      				local isrcx=srcx
      				if invert then
      						isrcx=w+1-srcx
      				end
          if band(bor(srcx,srcy),mask)==0 then
              local c=sget(sx+isrcx,sy+srcy)
              -- local dist=sqrt((ix-w/2)^2 + (iy-w/2)^2)
              if ((not ignore) or 
                 (not ignore(self,c,x,y,ix,iy,sx,sy,srcx,srcy))) then 
              		pset(x+ix,y+iy,c)
              end
          end
          srcx-=ddy0
          srcy+=ddx0
      end
      dx0+=ddx0
      dy0+=ddy0
  end
end

function turnable:ignore(c,x,y,ix,iy,sx,sy,srcx,srcy)
		return c==1
end

-------------------------------
-- entity: changeable
-------------------------------

changeable=turnable:extend({
			 sprchange=0
})

function changeable:render()
		self:renderrot(self.sprite, self.pos, self.a, self.size, self.inverted, self.ignore)
    local ignoreoverlay=self.ignoreoverlay
    local currchange=self:getcurrchange()
    if currchange <=0 or currchange>=1 then
    		ignoreoverlay=self.ignore
    end
    	self:renderrot(self.sprchange, self.pos, self.a, self.size, self.inverted, ignoreoverlay)
end

function changeable:getcurrchange() 
			return 0
end

function changeable:ignoreoverlay(c,x,y,ix,iy,sx,sy,srcx,srcy)
			return is_in_radius_from_sprite(ix,iy,self.size,self:getcurrchange()) or
									c==1
end

-- util
function is_in_radius_from_sprite(x,y,size,proportion)
		local center=(size*8-1)/2
		local r=size*8*proportion
		return r<sqrt((x-center)^2 + (y-center)^2)
end

-- util (could be removed for space optimization)
//function changeable:draw_with_mask(sprite,pos,size,ignore)  
//		local sx=8*sprite%128
//		local sy=flr(sprite/16)*8
//		local x=pos.x
//		local y=pos.y	
//		local w=8*size-1

//		for ix=0,w do
//				for iy=0,w do
//						local c=sget(sx+ix, sy+iy)
//						if not ignore or
//								not ignore(self,c,x,y,ix,iy,sx,sy,srcx,srcy) then
//								pset(x+ix, y+iy)
//						end
//				end
//		end
//end

-------------------------------
-- entity: spawner
-------------------------------

spawner=entity:extend({
			wave=1,
			enemycount=0,
			friendcount=0,
			actual_wave=0,
			draw_order=7,
			t_between_waves=90
})

function spawner:init()
		self:beginwave()
		self:become("updatewave")
end

function spawner:updatewave()
		if self.enemycount==0 
				and not self.stopchange then
				self:nextwave()
		elseif self.friendcount==0
				and self.player.level==1
				and not self.stopchange then
				self:nextwave()
		end
end

function spawner:wavestart()
	if self.t>self.t_between_waves then
		self:become("updatewave")
		self:beginwave()
	end
end

function spawner:nextwave()
		self.wave+=1
		
		if not self.ignore_wave_start then
			self.actual_wave+=1
			self:become("wavestart")
		else
			self:beginwave()
		end
end

function spawner:beginwave()
		for _,elem in pairs(waves[self.wave]) do
				for _=1,(elem.qty or 1) do
						e=elem.class:extend(elem.data()){}
						e.player=self.player
						if e.bullet then
								if e.bullet.dmg>0 then
										self.enemycount+=1
										e.ondestroy=event{
												origin=self,
												call=self.enemydestroyed
										}
								elseif e.bullet.dmg<0 then
										self.friendcount+=1
										e.ondestroy=event{
												origin=self,
												call=self.frienddestroyed
										}
								end
						end
						e_add(grave{
							pos=v(e.pos.x,e.pos.y),
							spawn=e
							})
				end
		end
end

function spawner:frienddestroyed()
		self.friendcount-=1
end

function spawner:enemydestroyed()
		self.enemycount-=1
end

function spawner:render()
	if self.state=="wavestart" then
		rectfill(0,45,64,51,13)
		print(
			"wave: " .. tostr(self.actual_wave),
			16,46,7
		)
	end
end

-------------------------------
-- entity: player
-------------------------------

player=turnable:extend({
				maxspeed=1,
				acc=.25,
				friction=.2,
    
    vel=v(0,0),
    t=0,

    draw_order=4,
    tags={"player"},
    
    level=1,
    changelevelamount=2,
    levelupmeter=0,
    leveldownmeter=0,
    
    
    visionradius=15
})

function player:init()
  self:become("begin")
  self:setlevel()
end

-- This update function will always get called
function player:update()
  
end

function player:begin()
		if btn(5) then
				self:setangle()
		end
		
		if btnp(4) then
  	self:shoot()
  end
end

-- This one is a state-specific update function
function player:walking()
  if not btn(5) then
  	self:setspeed()
  end
  	
  self:setangle()
  
  if btnp(4) then
  	self:shoot()
  end
end

function player:setspeed() 
	prev = v(self.vel.x, self.vel.y)
  if (btn(0)) self.vel.x -= self.acc
  if (btn(1)) self.vel.x += self.acc
  if (btn(2)) self.vel.y -= self.acc
  if (btn(3)) self.vel.y += self.acc
		
	norm=self.vel:norm()
 	len=min(self.vel:len(),
 	self.maxspeed) 
 
  if prev == self.vel then
  	len=approach(len, 0,
  		self.friction)
  end
 
 	self.vel=norm * len
end

function player:setangle()
	if (btn(0)) self.a = 180
  if (btn(1)) self.a = 0
  if (btn(2)) self.a = 270
  if (btn(3)) self.a = 90
  
  if (btn(0) and btn(2)) self.a = 225
  if (btn(0) and btn(3)) self.a = 135
  if (btn(1) and btn(2)) self.a = 315
  if (btn(1) and btn(3)) self.a = 45
end

function player:shoot()
  e_add(self.bullet{
    speed=3,
    pos=v(self.pos.x-self.size, self.pos.y-self.size),
    shtangle=self.a,
    origin=self})
end

function player:take_hit(dmg)
				valid=self:updatelevel(dmg)
				
    if not valid and not self.done then
      self.done=true
      invoke(function() change_state(deadstate, self.spawner.wave,self.spawner.t) end,30,self)
      explode(
        self.pos.x, self.pos.y,
        self.hitbox.xr-self.hitbox.xl,
        self.hitbox.yb-self.hitbox.yt)
				end
    
    if (dmg>0) shake+=10
end

function player:updatelevel(diff)
		if diff>0 then
				self.leveldownmeter+=diff
				if self.leveldownmeter>=self.changelevelamount then
						self.level-=flr(self.leveldownmeter/self.changelevelamount)
						self.leveldownmeter=0
						self.levelupmeter=0
				end
		else 
				self.levelupmeter-=diff
				if self.levelupmeter>=self.changelevelamount then
						self.level+=flr(self.levelupmeter/self.changelevelamount)
						self.levelupmeter=0
						self.leveldownmeter=0
				end
		end
  
  self.level=min(#playerlevel, self.level)
		if self.level>0 then
				self:setlevel()
				return true
		else
				return false
		end
end

function player:setlevel()
		level=playerlevel[self.level]
		self.sprite=level.sprite
		self.hitbox=level.hitbox
		self.bullet=level.bullet
		self.size=level.size
		self.maxspeed=level.maxspeed
end

-------------------------------
-- entity: bullet
-------------------------------

bullet=dynamic:extend({
		speed=1,
    hitbox=box(2,2,6,6),
    vel=v(0,0),
    t=0,
    lifetime=120,
    
    colors={9,8},
   	size=2,
   
    currchange=0,
    
    tags={"bullet"},
    collides_with={"player",
    															"npc"},
    dmg=1
})

function bullet:init()
		if self.shtangle ~= nil then
	  self.vel=v(
	    cos(self.shtangle/360),
	    -sin(self.shtangle/360))
		end
		
		self.vel *= self.speed
end

function bullet:render()
			local pos=v(self.pos.x+
				(self.hitbox.xl+self.hitbox.xr)/2,
				self.pos.y+
				(self.hitbox.yt+self.hitbox.yb)/2)

			for i=1,#self.colors do
				circfill(pos.x,pos.y,
													ceil(self.size/i), self.colors[i])
			end
			
			if self.currchange~=0 then
				circfill(pos.x,pos.y,
													self.size*self.currchange, 7)
				circfill(pos.x,pos.y,
													self.size*self.currchange/2, 6)
			end
end

function bullet:update()
  if (self.t > self.lifetime) self.done = true
end

function bullet:collide(e)
		if e~=self.origin then
			self.done=true
			self:explode()

      if (self.hit) self:hit(e)
		end
end

function bullet:getcurrchange()
		return self.currchange
end

function bullet:explode()
	sfx(1)
	for i=1,10 do
		add_new_dust(self.pos.x+4,self.pos.y+4,
			rnd(2)-1,rnd(2)-1,
			10,rnd(2)+1,0.05,0.9,
			{7,7,7,7,7,7,6,6,6,6,6,5,5,9,9,10,10,10,10,10,8,8,8,8})
	end
  shake += 5
end

function bullet:hit(e)
  if e.take_hit then
    e:take_hit(self.dmg)
  end
end

-------------------------------
-- entity: enemy bullet
-------------------------------

enemybullet=bullet:extend({
			sprite=3,
			collides_with={"player"}
})

function enemybullet:hit(e)
  if e:is_a("player") and e.take_hit then
    e:take_hit(self.dmg)
  end
end

-------------------------------
-- entity: friend bullet
-------------------------------

friendbullet=bullet:extend({
			colors={12,11},
   collides_with={"player"},
   dmg=-1
})

function friendbullet:explode()
	sfx(0)
	for i=1,10 do
		add_new_dust(self.pos.x+4,self.pos.y+4,
			rnd(1)-1,rnd(1)-1,
			10,rnd(3)+1,0.05,0.9,
			{11,11,11,11,11,11,3,3,3,3,3,5,5,3,3,11,11,11,11,11,12,12,12,12})
	end
end

function friendbullet:hit(e)
		if e:is_a("player") and e.take_hit then
	   e:take_hit(self.dmg)
	 end	
end

-------------------------------
-- entity: null bullet
-------------------------------

nullbullet=bullet:extend({
			sprite=18,
			dmg=0
})

function nullbullet:render()
		spr_render(self)
end

function nullbullet:hit(e)

end

function nullbullet:explode()
	add_new_dust(self.pos.x+4,self.pos.y+4,
			rnd(2)-1,rnd(2)-1,
			10,rnd(2)+1,0.05,0.9,
			10)
end

-------------------------------
-- entity: small bullet
-------------------------------

smallbullet=bullet:extend({
			sprite=19,
			dmg=1
})

-------------------------------
-- entity: large bullet
-------------------------------

largebullet=bullet:extend({
			colors={10,9,2},
			size=3,
			dmg=4
})

-------------------------------
-- entity: giga bullet
-------------------------------

gigabullet=bullet:extend({
			colors={10,8,14,2},
			dmg=4,
			size=5,
			hitbox=box(1,1,12,12)
})

function gigabullet:collide(e)
		if e~=self.origin then
   if (self.hit) self:hit(e)
		end
end

-------------------------------
-- movement types
-------------------------------

function linear_movement(e)
  if e.pos.x < -8 or e.pos.x > 64 
  or e.pos.y < -8 or e.pos.y > 64 then
    e.dir *= -1
  end
  e.vel = e.dir*e.speed
end

function sine_movement(e)
  local am = e.a or 5
  local sm = sin(e.t*e.aspeed) * am
  local d = v(-e.dir.y, e.dir.x)

  e.pos += d * sm
  linear_movement(e)
end

function point_movement(e)
  local c=e.cur_point
  local p=e.points[c]

  if (is_close(e.pos,p,e.speed)) e.cur_point+=1
  if (e.cur_point>#e.points)e.cur_point=1

  e.dir=v(p.x-e.pos.x,p.y-e.pos.y):norm()
  e.vel = e.dir*e.speed
end

-------------------------------
-- entity: npc
-------------------------------

npc=changeable:extend({
		speed=1,
    reloadtime=30,
    reloadthresh=10,
    randomshotoffset=10,
				
    hitbox=box(0,0,8,8),
    sprite=4,
    bullet=bullet,
    vel=v(0,0),
    dir=v(0,0),
    t=0,
    lastshot=0,
    player=nil,
    at=0,
    ft=5,
    asize=1,
    
    sprchange=7,
    changetime=180,
    
    tags={"npc"},
    collides_with={"player"},
    health=4,
    move=linear_movement,
    
    ondestroy=nil
})

function npc:init()
	self.s_beg=self.sprite
	self.s_end=self.sprite+self.asize
	self.sc_beg=self.sprchange
	self.sc_end=self.sprchange+self.asize
 self.dir = self.dir:norm()
end

-- This update function will always get called
function npc:update()
    if self.t-self.lastshot > self.reloadtime then
			self:shoot()
			self.lastshot = self.t
			 + rnd(self.reloadthresh)
			 --note: with or without random?
		end

    self.move(self)
end

function npc:render()
		if self.at>self.ft then
			self.at=0
			self.sprite+=1
			self.sprchange+=1
			if(self.sprite>=self.s_end)self.sprite=self.s_beg self.sprchange=self.sc_beg
		end
		
		self.at+=1

		self.inverted=self.dir.x>0
		local dist=self:getplayerdist(self.pos.x, self.pos.y)
		if dist>self.player.visionradius+self.size*8 then
				changeable.render(self)
		else
				self:renderrot(self.sprite, self.pos, self.a, self.size, self.inverted, self.ignore)
    self:renderrot(self.sprchange, self.pos, self.a, self.size, self.inverted, self.ignoreoverlay)
		end
end

function npc:shoot()
		direction = (self.player.pos
															-self.pos
															+v(
																rnd(self.randomshotoffset),
																rnd(self.randomshotoffset)))
															:norm()
		
		e_add(self.bullet{
    pos=v(self.pos.x, self.pos.y),
    vel=direction,
    origin=self,
    currchange=self:getcurrchange()})
end

function npc:getcurrchange() 
			return min(1, self.t/self.changetime)
end

function npc:take_hit(dmg)
  self.health-=dmg
  if(self.health <= 0) then
    self.done = true
    if self.ondestroy then
     	self.ondestroy:exec()
    end
    explode(
      self.pos.x, self.pos.y,
      self.hitbox.xr-self.hitbox.xl,
      self.hitbox.yb-self.hitbox.yt)
   end
end

function npc:ignoreoverlay(c,x,y,ix,iy,sx,sy,srcx,srcy)
		local dist=self:getplayerdist(x+ix,y+iy)

		return dist<=self.player.visionradius or
									changeable.ignoreoverlay(self,c,x,y,ix,iy,sx,sy,srcx,srcy)
end

function npc:getplayerdist(x,y)
		return v(self.player.pos.x-x,
											self.player.pos.y-y)
											:len()
end

-------------------------------
-- entity: enemy
-------------------------------

enemy=npc:extend({
    sprite=56,
    sprchange=24,
    asize=4,
    bullet=enemybullet
})

-------------------------------
-- entity: friend
-------------------------------

friend=npc:extend({
    sprite=40,
    asize=4,
    randomshotoffset=25,
    bullet=friendbullet,
    sprchange=24
})

-------------------------------
-- entity: friend tutorial
-------------------------------

friendtutorial=friend:extend({
		reloadtime=10000,
  randomshotoffset=0,
  speed=0,
  changetime=1800
})

function friendtutorial:init()
		friend:init(self)
		self:become("start")
end

function friendtutorial:start()
	invoke(function(e)
				e:shoot()
		end,80,self)
		
		invoke(function()
				self.done=true
		end,120,self)
		
		self:become("none")
end

-------------------------------
-- data: playerlevel
-------------------------------
playerlevel=object:extend({
					size=1,
					maxspeed=1
})

playerlevel={
				playerlevel{
					sprite=13,
					hitbox=box(3,3,4,4),
					bullet=nullbullet,
					maxspeed=2
				},
				playerlevel{
					sprite=2,
					hitbox=box(2,2,5,5),
					bullet=smallbullet,
				},
				playerlevel{
					sprite=14,
					hitbox=box(1,1,6,6),
					bullet=bullet,
				},
				playerlevel{
					sprite=15,
					hitbox=box(0,0,7,7),
					bullet=largebullet,
				},
				playerlevel{
					sprite=16,
					hitbox=box(0,0,10,10),
					bullet=gigabullet,
					size=2
				},
}

-------------------------------
-- data: waves
-------------------------------

waves={
		-- the void before tutorial
		{
		},
		-- tutorial
		{
				{
						class=friendtutorial,
						data=function() return {
								pos=v(0,screen_size/2),
								dir=v(1,0),
						} end
				},
				{
						class=friendtutorial,
						data=function() return {
								pos=v(screen_size-8,screen_size/2),
								dir=v(-1,0),
						} end
				},
		},
		{
				-- wave 1
				{
						class=enemy,
						data=function() return {
								pos=v(screen_size/2,0),
								speed=0,
								dir=v(-1,0),
        reloadtime=rnd(5) + 60,
        randomshotoffset=50,
        changetime=1000 
						} end
				},
		},
		{
		-- wave 2
				{
						class=enemy,
						data=function() return {
								pos=v(0,rnd(20)),
								speed=rnd(2),
								dir=v(-1,0),
                reloadtime=rnd(5) + 25,
                move=sine_movement,
                aspeed=0.05,
						} end
				},
				{
						class=friend,
						data=function() return {
								pos=v(screen_size,rnd(20)),
								speed=rnd(2),
								dir=v(-1,0),
                reloadtime=rnd(5) + 10,
                move=point_movement,
                points={v(8,8),v(8,56),v(56,56),v(56,8)},
                cur_point=1
						} end
				},
		},
		{
		-- wave 3
				{
						class=enemy,
						qty=2,
						data=function() return {
								pos=v(0,0),
								speed=rnd(2),
								dir=v(1,0),
                reloadtime=rnd(5) + 25
						} end
				},
				{
						class=friend,
						data=function() return {
								pos=v(screen_size,0),
								speed=rnd(2),
								dir=v(-1,0),
                reloadtime=rnd(5) + 10
						} end
				}
		}
}
-->8
grave=entity:extend({
	sprite=48,
	speed=0.3,
	offy=8
})

function grave:init()
	self:become("rising")
end

function grave:rising()
	-- spawn smoke particles
	for i=0,1 do
		add_new_dust(
			self.pos.x+rnd(8),self.pos.y+9,
			rnd(0.3)-0.15,-(rnd(0.3)+0.3),
			5,1,0.1,0,4
		)
	end
	-- move tombstone
	self.offy=approach(self.offy, 0, self.speed)
	if self.offy==0 then
		self.spawn.pos=v(self.pos.x, self.pos.y+8)
		self:become("spawning")
	end
end

function grave:spawning()
	self.spawn.pos.y=approach(
		self.spawn.pos.y, 
		self.pos.y,
		self.speed*2
	)
	
	if self.spawn.pos.y==self.pos.y then
		e_add(self.spawn)
		self:become("down")
	end
end

function grave:down()
	-- spawn smoke particles
	for i=0,1 do
		add_new_dust(
			self.pos.x+rnd(8),self.pos.y+9,
			rnd(0.3)-0.15,-(rnd(0.3)+0.3),
			5,1,0.1,0,4
		)
	end
	-- move tombstone
	self.offy=approach(self.offy, 8, self.speed)
	if self.offy==8 then
		self.done=true
	end
end

function grave:render()
	-- render tomb stone
	spr(self.sprite,
					self.pos.x,
					self.pos.y+self.offy)
	-- render spawns sprite
	if self.spawn and self.state=="spawning"then 
		spr(self.spawn.sprite,
						self.spawn.pos.x,
						self.spawn.pos.y)
	end
	-- render ground
	spr(self.sprite+1,
					self.pos.x,
					self.pos.y+8)
end


__gfx__
00000000000000001111111100000000000000000000000000000000000000000000000000000000000000000000000000000000111111110000011185551111
000000000066660010000011000aa0000888888000000000006666000666666000000000000000000007700000bbbb00000cc0001100011108550001855ccc11
00700700060000601085500100a99a00088888800066660006666660066666600000000000066000007667000b0bb0b000cbbc001106001100cccc0011c66675
0007700006c00c60100666010a9999a0080880800006600006066060060660600006600000666600076666700b0bb0b00cbbbbc01106601108566750855bb675
0007700006000060100666010a9999a00a8008a00060060006600660066006600006600000666600076666700ab00ba00cbbbbc01106601108566750855bb675
00700700060cc0601085500100a99a00088888800066660006666660066666600000000000066000007667000bbbbbb000cbbc001106001100cccc0011c66675
000000000066660010000011000aa0000888888000000000006666000666666000000000000000000007700000bbbb00000cc0001100011108550001855ccc11
00000000000000001111111100000000000000000000000000000000000000000000000000000000000000000000000000000000111111110000011185551111
11111111111111111111111111111111111aa1111111111111111111000000001777711111111111111111111111111100000000000000000000000000000000
1111111111111111111111111111111111a99a11111111aaaa111111000000007777771117777111111111111777711100000000000000000000000000000000
111111111111111111111111111aa1111a9999a111111aaaaaa11111000000007070771177777711177771117777771100000000000000000000000000000000
1188555551111111111aa11111a99a11a992299a111aaa8888aaa111000000007777771770707711777777117070771100000000000000000000000000000000
118855ccccc11111111aa11111a99a11a992299a111a88888888a111000000001777777777777711707077117777771100000000000000000000000000000000
111111ccc6c5111111111111111aa1111a9999a111aa888ee888aa11000000001117777117777777777777111777771100000000000000000000000000000000
11885cc666751111111111111111111111a99a111aa888e22e888aa1000000001111111111111111177777771177777700000000000000000000000000000000
1188553bbb7599111111111111111111111aa1111aa88e2222e88aa1000000001111111111111111111111111111111100000000000000000000000000000000
1188553bbb7599110000000000000000000000001aa88e2222e88aa1000000001bbbb11111111111111111111111111100000000000000000000000000000000
11885cc6667511110000000000000000000000001aa888e22e888aa100000000b0b0bb111bbbb111111111111bbbb11100000000000000000000000000000000
111111ccc6c5111100000000000000000000000011aa888ee888aa1100000000b0b0bb11b0b0bb111bbbb111b0b0bb1100000000000000000000000000000000
118855ccccc11111000000000000000000000000111a88888888a11100000000bbbbbb1bb0b0bb11b0b0bb11b0b0bb1100000000000000000000000000000000
1188555551111111000000000000000000000000111aaa8888aaa111000000001bbbbbbbbbbbbb11b0b0bb11bbbbbb1100000000000000000000000000000000
111111111111111100000000000000000000000011111aaaaaa1111100000000111bbbb11bbbbbbbbbbbbb111bbbbb1100000000000000000000000000000000
1111111111111111000000000000000000000000111111aaaa1111110000000011111111111111111bbbbbbb11bbbbbb00000000000000000000000000000000
11111111111111110000000000000000000000001111111111111111000000001111111111111111111111111111111100000000000000000000000000000000
11566611546464643334433333333333333333330000000000000000000000001888811111111111111111111111111100000000000000000000000000000000
15666661444444344433333333333433333333330000000000000000000000008888881118888111111111111888811100000000000000000000000000000000
56666666444555433333333333333333333333330000000000000000000000008080881188888811188881118888881100000000000000000000000000000000
56555566454444543334443333333333333333330000000000000000000000008808881880808811888888118080881100000000000000000000000000000000
56666666345555443343333334333333333333330000000000000000000000001888888888088811808088118808881100000000000000000000000000000000
56665556454334543333333433333333333333330000000000000000000000001118888118888888880888111888881100000000000000000000000000000000
56666666445545443333443333333433333333330000000000000000000000001111111111111111188888881188888800000000000000000000000000000000
56666666354444544333333333333333333333330000000000000000000000001111111111111111111111111111111100000000000000000000000000000000
__map__
3234343434323234000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3433323434343434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3434343434343334000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3434333432343434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3434343434343434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3433343334343434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3434343434333434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3432343434343434000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
00030000000000e0500b0500905007050050500b0501e0502b0501b0001c0001c0001a00019000190001c0001f000230000500001000230001c000120000f0000c0000a0000800007000070000f0001f0002b000
0001000010150101500f1500d1500c1500c1500c1500c1500f0501105012050120500a6500a6500a6500965007650000000000000000000000000000000000000000000000000000000000000000000000000000
